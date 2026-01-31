//! Simplify runtime prints of empty strings.

use std::collections::HashMap;

use crate::ssa::model::ir::{
    Block, Callee, CastKind, ConstValue, Function, InstKind, RuntimeFn, ValueId,
};
use crate::ssa::opt::Pass;

/// Replace empty-string `__rt_print` calls with direct null/zero arguments and
/// drop the now-redundant `__rt_string_drop`.
pub struct EmptyStringPrint;

impl Pass for EmptyStringPrint {
    fn name(&self) -> &'static str {
        "empty-string-print"
    }

    fn run(&mut self, func: &mut Function) -> bool {
        let mut changed = false;

        for block in func.blocks.iter_mut() {
            let mut defs = build_defs(block);
            let mut block_changed = false;

            for call_idx in 0..block.insts.len() {
                let Some((null_ptr, zero_u64, drop_idx)) =
                    match_empty_print(block, &defs, call_idx)
                else {
                    continue;
                };

                if let InstKind::Call { args, .. } = &mut block.insts[call_idx].kind {
                    args[0] = null_ptr;
                    args[1] = zero_u64;
                }
                block.insts.remove(drop_idx);
                changed = true;
                block_changed = true;
                break;
            }

            if block_changed {
                defs = build_defs(block);
            }

            let mut to_remove = Vec::new();
            for (idx, inst) in block.insts.iter().enumerate() {
                let drop_base = match &inst.kind {
                    InstKind::Call {
                        callee: Callee::Runtime(RuntimeFn::StringDrop),
                        args,
                    } if args.len() == 1 => args[0],
                    _ => continue,
                };
                if has_literal_init(block, &defs, drop_base) {
                    to_remove.push(idx);
                }
            }

            if !to_remove.is_empty() {
                for idx in to_remove.into_iter().rev() {
                    block.insts.remove(idx);
                }
                changed = true;
            }
        }

        changed
    }
}

fn build_defs(block: &Block) -> HashMap<ValueId, usize> {
    let mut defs = HashMap::new();
    for (idx, inst) in block.insts.iter().enumerate() {
        if let Some(result) = &inst.result {
            defs.insert(result.id, idx);
        }
    }
    defs
}

fn inst_of<'a>(
    block: &'a Block,
    defs: &HashMap<ValueId, usize>,
    id: ValueId,
) -> Option<&'a InstKind> {
    let idx = defs.get(&id)?;
    Some(&block.insts[*idx].kind)
}

fn match_empty_print(
    block: &Block,
    defs: &HashMap<ValueId, usize>,
    call_idx: usize,
) -> Option<(ValueId, ValueId, usize)> {
    let trace = std::env::var("MACHINA_TRACE_OPT").ok().as_deref() == Some("1");
    let call = block.insts.get(call_idx)?;
    let InstKind::Call { callee, args } = &call.kind else {
        return None;
    };
    if !matches!(callee, Callee::Runtime(RuntimeFn::Print)) || args.len() != 3 {
        return None;
    }

    let ptr_arg = args[0];
    let len_arg = args[1];

    let InstKind::Load { ptr: ptr_field } = inst_of(block, defs, ptr_arg)? else {
        if trace {
            eprintln!("[empty-string-print] ptr arg not a load");
        }
        return None;
    };
    let InstKind::FieldAddr {
        base: call_base,
        index,
    } = inst_of(block, defs, *ptr_field)?
    else {
        if trace {
            eprintln!("[empty-string-print] ptr field not field_addr");
        }
        return None;
    };
    if *index != 0 {
        if trace {
            eprintln!("[empty-string-print] ptr field index != 0");
        }
        return None;
    }

    let len_load = match inst_of(block, defs, len_arg)? {
        InstKind::IntExtend { value, .. } => *value,
        _ => {
            if trace {
                eprintln!("[empty-string-print] len arg not zext");
            }
            return None;
        }
    };
    let InstKind::Load { ptr: len_field } = inst_of(block, defs, len_load)? else {
        if trace {
            eprintln!("[empty-string-print] len load not load");
        }
        return None;
    };
    let InstKind::FieldAddr {
        base: base2,
        index: len_index,
    } = inst_of(block, defs, *len_field)?
    else {
        if trace {
            eprintln!("[empty-string-print] len field not field_addr");
        }
        return None;
    };
    if call_base != base2 || *len_index != 1 {
        if trace {
            eprintln!("[empty-string-print] len field base/index mismatch");
        }
        return None;
    }

    let empty_base = find_empty_base(block, defs, *call_base, call_idx)?;
    let (null_ptr, zero_u64) = empty_constants(block, defs, empty_base)?;

    let drop_idx = block
        .insts
        .iter()
        .enumerate()
        .skip(call_idx + 1)
        .find_map(|(idx, inst)| match &inst.kind {
            InstKind::Call {
                callee: Callee::Runtime(RuntimeFn::StringDrop),
                args,
            } if args.len() == 1 && args[0] == *call_base => Some(idx),
            _ => None,
        })?;

    Some((null_ptr, zero_u64, drop_idx))
}

fn find_empty_base(
    block: &Block,
    defs: &HashMap<ValueId, usize>,
    call_base: ValueId,
    call_idx: usize,
) -> Option<ValueId> {
    if has_empty_init(block, defs, call_base) {
        return Some(call_base);
    }

    for inst in block.insts.iter().take(call_idx) {
        let InstKind::Store { ptr, value } = inst.kind else {
            continue;
        };
        if ptr != call_base {
            continue;
        }
        let InstKind::Load { ptr: src } = inst_of(block, defs, value)? else {
            continue;
        };
        if has_empty_init(block, defs, *src) {
            return Some(*src);
        }
    }

    None
}

fn has_empty_init(block: &Block, defs: &HashMap<ValueId, usize>, base: ValueId) -> bool {
    let mut has_ptr = false;
    let mut has_len = false;
    let mut has_cap = false;
    for inst in &block.insts {
        let InstKind::Store { ptr, value } = inst.kind else {
            continue;
        };
        let Some(field_inst) = inst_of(block, defs, ptr) else {
            continue;
        };
        let InstKind::FieldAddr {
            base: store_base,
            index,
        } = field_inst
        else {
            continue;
        };
        if *store_base != base {
            continue;
        }
        match index {
            0 => has_ptr |= is_null_ptr_cast(block, defs, value).is_some(),
            1 => has_len |= is_const_zero(block, defs, value),
            2 => has_cap |= is_const_zero(block, defs, value),
            _ => {}
        }
    }
    has_ptr && has_len && has_cap
}

fn has_literal_init(block: &Block, defs: &HashMap<ValueId, usize>, base: ValueId) -> bool {
    let mut has_ptr = false;
    let mut has_len = false;
    let mut has_cap = false;
    for inst in &block.insts {
        let InstKind::Store { ptr, value } = inst.kind else {
            continue;
        };
        let Some(field_inst) = inst_of(block, defs, ptr) else {
            continue;
        };
        let InstKind::FieldAddr {
            base: store_base,
            index,
        } = field_inst
        else {
            continue;
        };
        if *store_base != base {
            continue;
        }
        match index {
            0 => has_ptr |= is_global_addr(block, defs, value),
            1 => has_len |= is_const_int(block, defs, value),
            2 => has_cap |= is_const_zero(block, defs, value),
            _ => {}
        }
    }
    has_ptr && has_len && has_cap
}

fn empty_constants(
    block: &Block,
    defs: &HashMap<ValueId, usize>,
    base: ValueId,
) -> Option<(ValueId, ValueId)> {
    let mut null_ptr = None;
    let mut zero_val = None;
    for inst in &block.insts {
        let InstKind::Store { ptr, value } = inst.kind else {
            continue;
        };
        let Some(field_inst) = inst_of(block, defs, ptr) else {
            continue;
        };
        let InstKind::FieldAddr {
            base: store_base,
            index,
        } = field_inst
        else {
            continue;
        };
        if *store_base == base && *index == 0 {
            if let Some((ptr_id, zero_id)) = is_null_ptr_cast(block, defs, value) {
                null_ptr = Some(ptr_id);
                zero_val = Some(zero_id);
            }
            break;
        }
    }
    let null_ptr = null_ptr?;
    let zero_val = zero_val?;
    let zero_u64 = if is_u64_zero(block, defs, zero_val) {
        zero_val
    } else {
        find_u64_zero(block)?
    };
    Some((null_ptr, zero_u64))
}

fn is_const_zero(block: &Block, defs: &HashMap<ValueId, usize>, value: ValueId) -> bool {
    let Some(inst) = inst_of(block, defs, value) else {
        return false;
    };
    matches!(
        inst,
        InstKind::Const {
            value: ConstValue::Int { value: 0, .. }
        }
    )
}

fn is_const_int(block: &Block, defs: &HashMap<ValueId, usize>, value: ValueId) -> bool {
    matches!(
        inst_of(block, defs, value),
        Some(InstKind::Const {
            value: ConstValue::Int { .. }
        })
    )
}

fn is_global_addr(block: &Block, defs: &HashMap<ValueId, usize>, value: ValueId) -> bool {
    matches!(
        inst_of(block, defs, value),
        Some(InstKind::Const {
            value: ConstValue::GlobalAddr { .. }
        })
    )
}

fn is_null_ptr_cast(
    block: &Block,
    defs: &HashMap<ValueId, usize>,
    ptr_id: ValueId,
) -> Option<(ValueId, ValueId)> {
    let Some(inst) = inst_of(block, defs, ptr_id) else {
        return None;
    };
    match inst {
        InstKind::Cast {
            kind: CastKind::IntToPtr,
            value,
            ..
        } if is_const_zero(block, defs, *value) => Some((ptr_id, *value)),
        _ => None,
    }
}

fn is_u64_zero(block: &Block, defs: &HashMap<ValueId, usize>, value: ValueId) -> bool {
    match inst_of(block, defs, value) {
        Some(InstKind::Const {
            value: ConstValue::Int { value: 0, bits, .. },
        }) => *bits == 64,
        _ => false,
    }
}

fn find_u64_zero(block: &Block) -> Option<ValueId> {
    block.insts.iter().find_map(|inst| match &inst.kind {
        InstKind::Const {
            value: ConstValue::Int { value: 0, bits, .. },
        } if *bits == 64 => inst.result.as_ref().map(|result| result.id),
        _ => None,
    })
}
