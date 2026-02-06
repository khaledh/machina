use super::*;

impl TypeChecker {
    pub(super) fn resolve_struct_field<'a>(
        &self,
        struct_ty: &'a Type,
        field: &str,
    ) -> Option<&'a StructField> {
        match struct_ty {
            Type::Struct { fields, .. } => fields.iter().find(|f| f.name == field),
            _ => None,
        }
    }

    pub(super) fn resolve_struct_type_for_update(&self, ty: Type) -> Option<Type> {
        match &ty {
            Type::Struct { .. } => Some(ty),
            _ => None,
        }
    }

    pub(super) fn resolve_enum_variant<'a>(
        &self,
        enum_ty: &'a Type,
        variant_name: &str,
    ) -> Option<&'a EnumVariant> {
        match enum_ty {
            Type::Enum { variants, .. } => variants.iter().find(|v| v.name == variant_name),
            _ => None,
        }
    }

    pub(super) fn resolve_enum_variant_in<'a>(
        &self,
        variants: &'a [EnumVariant],
        variant_name: &str,
    ) -> Option<&'a EnumVariant> {
        variants.iter().find(|v| v.name == variant_name)
    }
}
