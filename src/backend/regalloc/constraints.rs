//! ABI constraints for SSA register allocation.
//!
//! This module computes precoloring constraints that force certain SSA values
//! into specific physical registers based on the target ABI. It also identifies
//! stack-passed parameters that need special handling.
//!
//! # Precoloring Strategy
//!
//! The allocator tries to precolor values to their ABI-mandated registers when
//! it's safe to do so. A value can be precolored if:
//!
//! - **Return values**: Placed in the result register if the value doesn't
//!   cross a call site (which would clobber caller-saved registers)
//!
//! - **Entry parameters**: Placed in their ABI parameter registers if:
//!   - The value doesn't cross a call site, OR
//!   - The register is callee-saved (preserved across calls)
//!
//! Values that fail these checks are allocated normally and moved to/from
//! ABI registers at call boundaries via the move planning phase.
//!
//! # Incoming Stack Arguments
//!
//! Parameters beyond the register limit are passed on the stack. This module
//! maps these values to their SP-relative offsets so the allocator can emit
//! loads from the correct locations.

use std::collections::{HashMap, HashSet};

use crate::backend::IrTypeCache;
use crate::backend::regalloc::Location;
use crate::backend::regalloc::moves;
use crate::backend::regalloc::target::{PhysReg, TargetSpec};
use crate::ir::ir::ValueId;

use super::intervals::{IntervalAnalysis, LiveInterval};

// ============================================================================
// Data Structures
// ============================================================================

/// Precomputed ABI constraints for a function.
///
/// Contains information needed by the register allocator to respect the
/// target calling convention.
pub struct AbiConstraints {
    /// Number of parameter registers available on the target.
    pub param_reg_count: usize,
    /// Values precolored to specific physical registers.
    pub fixed_regs: HashMap<ValueId, PhysReg>,
    /// Stack-passed parameters mapped to their SP-relative byte offsets.
    pub incoming_args: HashMap<ValueId, u32>,
}

// ============================================================================
// Constraint Building
// ============================================================================

/// Builds ABI constraints from interval analysis and target specification.
///
/// This function:
/// 1. Precolors return values to the result register (when safe)
/// 2. Precolors entry parameters to their ABI registers (when safe)
/// 3. Maps stack-passed parameters to their SP-relative offsets
///
/// "Safe" means the value's live interval doesn't cross a call site where
/// caller-saved registers would be clobbered.
pub fn build(
    analysis: &IntervalAnalysis,
    types: &mut IrTypeCache,
    target: &dyn TargetSpec,
) -> AbiConstraints {
    let param_reg_count = param_reg_count(target);
    let caller_saved: HashSet<PhysReg> = target.caller_saved().iter().copied().collect();
    let param_set: HashSet<ValueId> = analysis.param_values.iter().copied().collect();

    let mut fixed_regs = HashMap::new();

    // Precolor return values into the ABI result register when safe.
    let result_reg = target.result_reg();
    for value in &analysis.return_values {
        let ty = analysis
            .value_types
            .get(value)
            .copied()
            .unwrap_or_else(|| panic!("backend regalloc: missing return type for {:?}", value));
        let pass = moves::arg_pass_info(types, ty);
        if !param_set.contains(value) && pass.kind == moves::PassKind::Reg {
            let interval = analysis
                .intervals
                .get(value)
                .copied()
                .unwrap_or(LiveInterval { start: 0, end: 0 });
            let crosses_call = interval_crosses_call(interval, &analysis.call_positions);
            if !crosses_call {
                fixed_regs.insert(*value, result_reg);
            }
        }
    }

    let mut next_reg = 0usize;
    let mut next_stack = 0u32;

    // Precolor entry parameters into their ABI registers when safe.
    for value in &analysis.param_values {
        if fixed_regs.contains_key(value) {
            continue;
        }
        let ty = analysis
            .value_types
            .get(value)
            .copied()
            .unwrap_or_else(|| panic!("backend regalloc: missing param type for {:?}", value));
        let pass = moves::arg_pass_info(types, ty);
        let src_locs = moves::abi_arg_locations(
            pass,
            param_reg_count,
            target,
            &mut next_reg,
            &mut next_stack,
            moves::ArgStackKind::Incoming,
        );
        if pass.kind == moves::PassKind::Reg {
            if let Some(Location::Reg(reg)) = src_locs.first().copied() {
                let interval = analysis
                    .intervals
                    .get(value)
                    .copied()
                    .unwrap_or(LiveInterval { start: 0, end: 0 });
                let crosses_call = interval_crosses_call(interval, &analysis.call_positions);
                if !(crosses_call && caller_saved.contains(&reg)) {
                    fixed_regs.insert(*value, reg);
                }
            }
        }
    }

    // Map stack-passed incoming params to their SP-relative offsets.
    let mut incoming_args = HashMap::new();
    let mut next_reg = 0usize;
    let mut next_stack = 0u32;
    for value in &analysis.param_values {
        let ty = analysis
            .value_types
            .get(value)
            .copied()
            .unwrap_or_else(|| panic!("backend regalloc: missing param type for {:?}", value));
        let pass = moves::arg_pass_info(types, ty);
        let src_locs = moves::abi_arg_locations(
            pass,
            param_reg_count,
            target,
            &mut next_reg,
            &mut next_stack,
            moves::ArgStackKind::Incoming,
        );
        if pass.kind == moves::PassKind::Reg {
            if let Some(Location::IncomingArg(offset)) = src_locs.first().copied() {
                incoming_args.insert(*value, offset);
            }
        }
    }

    AbiConstraints {
        param_reg_count,
        fixed_regs,
        incoming_args,
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Returns true if the live interval spans any call instruction.
///
/// Used to determine if a value in a caller-saved register would be clobbered.
fn interval_crosses_call(interval: LiveInterval, call_positions: &[u32]) -> bool {
    call_positions
        .iter()
        .any(|pos| *pos >= interval.start && *pos <= interval.end)
}

/// Counts the number of distinct parameter registers available on the target.
///
/// Stops counting when `param_reg` returns `None` or a duplicate register.
fn param_reg_count(target: &dyn TargetSpec) -> usize {
    const MAX_PARAM_REGS: u32 = 32;
    let mut count = 0usize;
    let mut seen = HashSet::new();
    for idx in 0..MAX_PARAM_REGS {
        match target.param_reg(idx) {
            Some(reg) => {
                if !seen.insert(reg) {
                    break;
                }
                count += 1;
            }
            None => break,
        }
    }
    count
}
