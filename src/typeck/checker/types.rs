use super::*;

impl TypeChecker {
    pub(super) fn sig_has_type_vars(sig: &OverloadSig) -> bool {
        sig.type_param_count > 0
            || sig
                .params
                .iter()
                .any(|param| Self::type_has_vars(&param.ty))
            || Self::type_has_vars(&sig.ret_ty)
    }

    pub(super) fn apply_inst_to_sig(
        sig: &OverloadSig,
        inst: &GenericInst,
    ) -> (Vec<Type>, Vec<ParamMode>, Type) {
        let mut unifier = Unifier::new();
        for (index, ty) in inst.type_args.iter().enumerate() {
            let var = TyVarId::new(index as u32);
            {
                let _ = unifier.unify(&Type::Var(var), ty);
            }
        }

        let param_types = sig
            .params
            .iter()
            .map(|param| {
                let ty = unifier.apply(&param.ty);
                Self::rewrite_nominal_names(&ty, &inst.type_args)
            })
            .collect::<Vec<_>>();
        let param_modes = sig
            .params
            .iter()
            .map(|param| param.mode.clone())
            .collect::<Vec<_>>();
        let ret_type = unifier.apply(&sig.ret_ty);
        let ret_type = Self::rewrite_nominal_names(&ret_type, &inst.type_args);
        (param_types, param_modes, ret_type)
    }

    pub(super) fn type_has_vars(ty: &Type) -> bool {
        match ty {
            Type::Var(_) => true,
            Type::Array { elem_ty, .. }
            | Type::Slice { elem_ty }
            | Type::Heap { elem_ty }
            | Type::Ref { elem_ty, .. }
            | Type::Range { elem_ty } => Self::type_has_vars(elem_ty),
            Type::Tuple { field_tys } => field_tys.iter().any(Self::type_has_vars),
            Type::Struct { fields, .. } => {
                fields.iter().any(|field| Self::type_has_vars(&field.ty))
            }
            Type::Enum { variants, .. } => variants
                .iter()
                .any(|variant| variant.payload.iter().any(Self::type_has_vars)),
            Type::Fn { params, ret_ty } => {
                params.iter().any(|param| Self::type_has_vars(&param.ty))
                    || Self::type_has_vars(ret_ty)
            }
            _ => false,
        }
    }

    fn rewrite_nominal_names(ty: &Type, type_args: &[Type]) -> Type {
        match ty {
            Type::Struct { name, fields } => Type::Struct {
                name: Self::subst_type_vars_in_name(name, type_args),
                fields: fields
                    .iter()
                    .map(|field| StructField {
                        name: field.name.clone(),
                        ty: Self::rewrite_nominal_names(&field.ty, type_args),
                    })
                    .collect(),
            },
            Type::Enum { name, variants } => Type::Enum {
                name: Self::subst_type_vars_in_name(name, type_args),
                variants: variants
                    .iter()
                    .map(|variant| EnumVariant {
                        name: variant.name.clone(),
                        payload: variant
                            .payload
                            .iter()
                            .map(|ty| Self::rewrite_nominal_names(ty, type_args))
                            .collect(),
                    })
                    .collect(),
            },
            Type::Array { elem_ty, dims } => Type::Array {
                elem_ty: Box::new(Self::rewrite_nominal_names(elem_ty, type_args)),
                dims: dims.clone(),
            },
            Type::Slice { elem_ty } => Type::Slice {
                elem_ty: Box::new(Self::rewrite_nominal_names(elem_ty, type_args)),
            },
            Type::Heap { elem_ty } => Type::Heap {
                elem_ty: Box::new(Self::rewrite_nominal_names(elem_ty, type_args)),
            },
            Type::Ref { mutable, elem_ty } => Type::Ref {
                mutable: *mutable,
                elem_ty: Box::new(Self::rewrite_nominal_names(elem_ty, type_args)),
            },
            Type::Range { elem_ty } => Type::Range {
                elem_ty: Box::new(Self::rewrite_nominal_names(elem_ty, type_args)),
            },
            Type::Tuple { field_tys } => Type::Tuple {
                field_tys: field_tys
                    .iter()
                    .map(|ty| Self::rewrite_nominal_names(ty, type_args))
                    .collect(),
            },
            Type::Fn { params, ret_ty } => Type::Fn {
                params: params
                    .iter()
                    .map(|param| FnParam {
                        mode: param.mode,
                        ty: Self::rewrite_nominal_names(&param.ty, type_args),
                    })
                    .collect(),
                ret_ty: Box::new(Self::rewrite_nominal_names(ret_ty, type_args)),
            },
            _ => ty.clone(),
        }
    }

    fn subst_type_vars_in_name(name: &str, type_args: &[Type]) -> String {
        let mut out = String::with_capacity(name.len());
        let mut chars = name.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == 'T' {
                let mut digits = String::new();
                while let Some(next) = chars.peek() {
                    if next.is_ascii_digit() {
                        digits.push(*next);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if !digits.is_empty() {
                    if let Ok(index) = digits.parse::<usize>() {
                        if let Some(ty) = type_args.get(index) {
                            out.push_str(&Self::type_arg_name(ty));
                            continue;
                        }
                    }
                    out.push('T');
                    out.push_str(&digits);
                    continue;
                }
            }
            out.push(ch);
        }
        out
    }

    fn type_arg_name(ty: &Type) -> String {
        match ty {
            Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
            Type::Var(var) => format!("T{}", var.index()),
            _ => ty.to_string(),
        }
    }

    pub(super) fn type_contains_param_var(ty: &Type, type_param_count: usize) -> bool {
        let param_limit = type_param_count as u32;
        match ty {
            Type::Var(var) => var.index() < param_limit,
            Type::Array { elem_ty, .. }
            | Type::Slice { elem_ty }
            | Type::Heap { elem_ty }
            | Type::Ref { elem_ty, .. }
            | Type::Range { elem_ty } => Self::type_contains_param_var(elem_ty, type_param_count),
            Type::Tuple { field_tys } => field_tys
                .iter()
                .any(|ty| Self::type_contains_param_var(ty, type_param_count)),
            Type::Struct { fields, .. } => fields
                .iter()
                .any(|field| Self::type_contains_param_var(&field.ty, type_param_count)),
            Type::Enum { variants, .. } => variants.iter().any(|variant| {
                variant
                    .payload
                    .iter()
                    .any(|ty| Self::type_contains_param_var(ty, type_param_count))
            }),
            Type::Fn { params, ret_ty } => {
                params
                    .iter()
                    .any(|param| Self::type_contains_param_var(&param.ty, type_param_count))
                    || Self::type_contains_param_var(ret_ty, type_param_count)
            }
            _ => false,
        }
    }

    pub(super) fn replace_param_vars_with_infer(
        &mut self,
        ty: &Type,
        type_param_count: usize,
        replacements: &mut HashMap<u32, TyVarId>,
    ) -> Type {
        let param_limit = type_param_count as u32;
        match ty {
            Type::Var(var) if var.index() < param_limit => {
                let entry = replacements
                    .entry(var.index())
                    .or_insert_with(|| self.new_infer_var());
                Type::Var(*entry)
            }
            Type::Array { elem_ty, dims } => Type::Array {
                elem_ty: Box::new(self.replace_param_vars_with_infer(
                    elem_ty,
                    type_param_count,
                    replacements,
                )),
                dims: dims.clone(),
            },
            Type::Slice { elem_ty } => Type::Slice {
                elem_ty: Box::new(self.replace_param_vars_with_infer(
                    elem_ty,
                    type_param_count,
                    replacements,
                )),
            },
            Type::Heap { elem_ty } => Type::Heap {
                elem_ty: Box::new(self.replace_param_vars_with_infer(
                    elem_ty,
                    type_param_count,
                    replacements,
                )),
            },
            Type::Ref { mutable, elem_ty } => Type::Ref {
                mutable: *mutable,
                elem_ty: Box::new(self.replace_param_vars_with_infer(
                    elem_ty,
                    type_param_count,
                    replacements,
                )),
            },
            Type::Range { elem_ty } => Type::Range {
                elem_ty: Box::new(self.replace_param_vars_with_infer(
                    elem_ty,
                    type_param_count,
                    replacements,
                )),
            },
            Type::Tuple { field_tys } => Type::Tuple {
                field_tys: field_tys
                    .iter()
                    .map(|ty| {
                        self.replace_param_vars_with_infer(ty, type_param_count, replacements)
                    })
                    .collect(),
            },
            Type::Struct { name, fields } => Type::Struct {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|field| StructField {
                        name: field.name.clone(),
                        ty: self.replace_param_vars_with_infer(
                            &field.ty,
                            type_param_count,
                            replacements,
                        ),
                    })
                    .collect(),
            },
            Type::Enum { name, variants } => Type::Enum {
                name: name.clone(),
                variants: variants
                    .iter()
                    .map(|variant| EnumVariant {
                        name: variant.name.clone(),
                        payload: variant
                            .payload
                            .iter()
                            .map(|ty| {
                                self.replace_param_vars_with_infer(
                                    ty,
                                    type_param_count,
                                    replacements,
                                )
                            })
                            .collect(),
                    })
                    .collect(),
            },
            Type::Fn { params, ret_ty } => Type::Fn {
                params: params
                    .iter()
                    .map(|param| FnParam {
                        mode: param.mode,
                        ty: self.replace_param_vars_with_infer(
                            &param.ty,
                            type_param_count,
                            replacements,
                        ),
                    })
                    .collect(),
                ret_ty: Box::new(self.replace_param_vars_with_infer(
                    ret_ty,
                    type_param_count,
                    replacements,
                )),
            },
            _ => ty.clone(),
        }
    }
}
