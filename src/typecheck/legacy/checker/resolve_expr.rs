use super::*;

#[derive(Clone, Copy)]
pub(super) struct ResolvedStruct<'a> {
    fields: &'a [StructField],
}

impl<'a> ResolvedStruct<'a> {
    pub(super) fn field(self, name: &str) -> Option<&'a StructField> {
        self.fields.iter().find(|field| field.name == name)
    }
}

#[derive(Clone, Copy)]
pub(super) struct ResolvedEnum<'a> {
    variants: &'a [EnumVariant],
}

impl<'a> ResolvedEnum<'a> {
    pub(super) fn variant(self, name: &str) -> Option<ResolvedVariant<'a>> {
        self.variants
            .iter()
            .find(|variant| variant.name == name)
            .map(|variant| ResolvedVariant { variant })
    }
}

#[derive(Clone, Copy)]
pub(super) struct ResolvedVariant<'a> {
    variant: &'a EnumVariant,
}

impl<'a> ResolvedVariant<'a> {
    pub(super) fn payload(self) -> &'a [Type] {
        &self.variant.payload
    }
}

#[derive(Clone, Copy)]
pub(super) enum ResolvedTypeDef<'a> {
    Struct(ResolvedStruct<'a>),
    Enum(ResolvedEnum<'a>),
}

impl<'a> ResolvedTypeDef<'a> {
    pub(super) fn as_struct(self) -> Option<ResolvedStruct<'a>> {
        match self {
            ResolvedTypeDef::Struct(def) => Some(def),
            ResolvedTypeDef::Enum(_) => None,
        }
    }

    pub(super) fn as_enum(self) -> Option<ResolvedEnum<'a>> {
        match self {
            ResolvedTypeDef::Struct(_) => None,
            ResolvedTypeDef::Enum(def) => Some(def),
        }
    }
}

impl TypeChecker {
    pub(super) fn resolve_struct_field<'a>(
        &self,
        struct_ty: &'a Type,
        field: &str,
    ) -> Option<&'a StructField> {
        self.resolve_struct(struct_ty)
            .and_then(move |resolved| resolved.field(field))
    }

    pub(super) fn resolve_struct_type_for_update(&self, ty: Type) -> Option<Type> {
        match &ty {
            Type::Struct { .. } => Some(ty),
            _ => None,
        }
    }

    pub(super) fn resolve_struct<'a>(&self, ty: &'a Type) -> Option<ResolvedStruct<'a>> {
        match ty {
            Type::Struct { fields, .. } => Some(ResolvedStruct { fields }),
            _ => None,
        }
    }

    pub(super) fn resolve_enum<'a>(&self, ty: &'a Type) -> Option<ResolvedEnum<'a>> {
        match ty {
            Type::Enum { variants, .. } => Some(ResolvedEnum { variants }),
            _ => None,
        }
    }

    pub(super) fn resolve_type_def<'a>(&self, ty: &'a Type) -> Option<ResolvedTypeDef<'a>> {
        if let Some(struct_def) = self.resolve_struct(ty) {
            return Some(ResolvedTypeDef::Struct(struct_def));
        }
        if let Some(enum_def) = self.resolve_enum(ty) {
            return Some(ResolvedTypeDef::Enum(enum_def));
        }
        None
    }

    pub(super) fn resolve_enum_variant_in<'a>(
        &self,
        enum_name: &'a str,
        variants: &'a [EnumVariant],
        variant_name: &str,
    ) -> Option<ResolvedVariant<'a>> {
        let _ = enum_name;
        variants
            .iter()
            .find(|variant| variant.name == variant_name)
            .map(|variant| ResolvedVariant { variant })
    }
}
