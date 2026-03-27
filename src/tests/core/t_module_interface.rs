use crate::core::api::parse_module_with_id_gen;
use crate::core::ast::NodeIdGen;
use crate::core::capsule::bind::CapsuleBindings;
use crate::core::capsule::{
    CapsuleParseOptions, FsModuleLoader, ModuleId, ModulePath,
    discover_and_parse_capsule_with_loader_and_options,
};
use crate::core::context::{
    CapsuleParsedContext, ParsedContext, module_export_facts_from_interface,
};
use crate::core::interface::{
    CallableImplementation, CallableSignature, ExportVisibility, ExportedDefKind,
    GenericClosureTemplate, GenericFunctionTemplate, GenericTemplateGraph,
    JsonModuleInterfaceCodec, LinkTimeCallableTemplate, ModuleArtifactPaths, ModuleInterface,
    ModuleInterfaceCodec, TemplateBinding, TemplateBindingId, TemplateBindingKind, TemplateBody,
    TemplateCallSite, TemplateCallTarget, TemplateDef, TemplateDefId, TemplateDefKind,
    TemplateExpr, TemplateIterableParamSlot, TemplateNestedClosure, TemplateReference,
    TemplateReferenceKind, TemplateReferenceTarget, TemplateSiteId, TemplateStructField,
    TemplateTypeParam, TemplateTypeParamId, TemplateTypeSite, TemplateTypeSiteRole,
    emit_module_interface_with_codec, interface_rel_path, load_stdlib_module_interface_with_codec,
    object_rel_path, read_module_interface_with_codec,
};
use crate::core::resolve::resolve;
use crate::core::symbol_id::{SymbolNs, SymbolPath, TypeKey};
use indoc::indoc;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};

static INTERFACE_TMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn resolved_with_module_path(
    source: &str,
    module_path: &str,
) -> crate::core::context::ResolvedContext {
    let id_gen = NodeIdGen::new();
    let (module, id_gen) = parse_module_with_id_gen(source, id_gen).expect("parse should succeed");
    let parsed = ParsedContext::new(module, id_gen).with_module_path(
        ModulePath::new(module_path.split("::").map(|s| s.to_string()).collect()).unwrap(),
    );
    resolve(parsed).expect("resolve should succeed")
}

#[test]
fn generic_template_graph_separates_local_and_external_identity() {
    let external_symbol = crate::core::symbol_id::SymbolId::new(
        ModulePath::new(vec!["std".into(), "iter".into()]).unwrap(),
        SymbolPath::from_names(["private_helper"]),
        SymbolNs::Value,
    );
    let root = TemplateDef {
        id: TemplateDefId(1),
        symbol_id: Some(crate::core::symbol_id::SymbolId::new(
            ModulePath::new(vec!["std".into(), "iter".into()]).unwrap(),
            SymbolPath::from_names(["map"]),
            SymbolNs::Value,
        )),
        kind: TemplateDefKind::Function(GenericFunctionTemplate {
            signature: CallableSignature {
                name: "map".to_string(),
                type_params: vec!["S".to_string(), "In".to_string(), "Out".to_string()],
                params: Vec::new(),
                ret_ty: TypeKey::Named {
                    module: ModulePath::new(vec!["std".into(), "iter".into()]).unwrap(),
                    path: SymbolPath::from_names(["MapIter"]),
                    args: Vec::new(),
                },
            },
            type_params: vec![
                TemplateTypeParam {
                    id: TemplateTypeParamId(0),
                    name: "S".to_string(),
                    bound: None,
                },
                TemplateTypeParam {
                    id: TemplateTypeParamId(1),
                    name: "In".to_string(),
                    bound: None,
                },
            ],
            body: TemplateBody {
                expr: None,
                params: Vec::new(),
                locals: Vec::new(),
                nested_closures: Vec::new(),
                call_sites: Vec::new(),
                type_sites: Vec::new(),
                iterable_param_slots: Vec::new(),
                references: vec![
                    TemplateReference {
                        target: TemplateReferenceTarget::Local(TemplateDefId(2)),
                        kind: TemplateReferenceKind::Type,
                    },
                    TemplateReference {
                        target: TemplateReferenceTarget::External(external_symbol.clone()),
                        kind: TemplateReferenceKind::Callable,
                    },
                ],
            },
        }),
    };
    let helper = TemplateDef {
        id: TemplateDefId(2),
        symbol_id: None,
        kind: TemplateDefKind::LinkTimeCallable(LinkTimeCallableTemplate {
            signature: CallableSignature {
                name: "private_helper".to_string(),
                type_params: Vec::new(),
                params: Vec::new(),
                ret_ty: TypeKey::Unit,
            },
            link_symbol: "__mc_std_iter_private_helper".to_string(),
            references: Vec::new(),
        }),
    };

    let graph = GenericTemplateGraph {
        root: TemplateDefId(1),
        defs: vec![root, helper],
    };

    assert_eq!(graph.root_def().map(|def| def.id), Some(TemplateDefId(1)));
    assert_eq!(
        graph.local_dependencies().collect::<Vec<_>>(),
        vec![TemplateDefId(2)]
    );
    assert_eq!(
        graph.external_dependencies().cloned().collect::<Vec<_>>(),
        vec![external_symbol]
    );
}

#[test]
fn template_body_tracks_monomorphize_payload_anchors() {
    let body = TemplateBody {
        expr: Some(TemplateExpr::BindingRef(TemplateBindingId(1))),
        params: vec![TemplateBinding {
            id: TemplateBindingId(1),
            name: "source".to_string(),
            kind: TemplateBindingKind::Param,
            ty: Some(TypeKey::Named {
                module: ModulePath::new(vec!["std".into(), "iter".into()]).unwrap(),
                path: SymbolPath::from_names(["MapIter"]),
                args: vec![TypeKey::GenericParam(0), TypeKey::GenericParam(1)],
            }),
        }],
        locals: vec![TemplateBinding {
            id: TemplateBindingId(2),
            name: "value".to_string(),
            kind: TemplateBindingKind::LocalVar,
            ty: Some(TypeKey::GenericParam(1)),
        }],
        nested_closures: vec![TemplateNestedClosure {
            def: TemplateDefId(9),
            captures: vec![TemplateBindingId(2)],
        }],
        call_sites: vec![TemplateCallSite {
            site: TemplateSiteId(3),
            callee: TemplateCallTarget::Def(TemplateReferenceTarget::External(
                crate::core::symbol_id::SymbolId::new(
                    ModulePath::new(vec!["std".into(), "iter".into()]).unwrap(),
                    SymbolPath::from_names(["map"]),
                    SymbolNs::Value,
                ),
            )),
            explicit_type_arg_count: 3,
            iterable_arg_count: 1,
        }],
        type_sites: vec![TemplateTypeSite {
            site: TemplateSiteId(4),
            role: TemplateTypeSiteRole::Local,
            ty: TypeKey::GenericParam(1),
        }],
        iterable_param_slots: vec![TemplateIterableParamSlot {
            inst_index: 0,
            binding: TemplateBindingId(1),
            item_ty: TypeKey::GenericParam(1),
        }],
        references: vec![TemplateReference {
            target: TemplateReferenceTarget::Local(TemplateDefId(9)),
            kind: TemplateReferenceKind::Callable,
        }],
    };

    assert_eq!(
        body.expr,
        Some(TemplateExpr::BindingRef(TemplateBindingId(1)))
    );
    assert_eq!(body.params.len(), 1);
    assert_eq!(body.locals.len(), 1);
    assert_eq!(body.nested_closures[0].def, TemplateDefId(9));
    assert_eq!(body.nested_closures[0].captures, vec![TemplateBindingId(2)]);
    assert_eq!(body.call_sites[0].site, TemplateSiteId(3));
    assert_eq!(body.iterable_param_slots[0].binding, TemplateBindingId(1));
    assert_eq!(body.iterable_param_slots[0].inst_index, 0);
    assert_eq!(body.type_sites[0].role, TemplateTypeSiteRole::Local);
}

#[test]
fn closure_template_uses_capture_bindings_from_parent_body() {
    let closure = TemplateDef {
        id: TemplateDefId(9),
        symbol_id: None,
        kind: TemplateDefKind::Closure(GenericClosureTemplate {
            captures: vec![TemplateBindingId(2)],
            body: TemplateBody {
                expr: Some(TemplateExpr::StructLit {
                    type_symbol: crate::core::symbol_id::SymbolId::new(
                        ModulePath::new(vec!["std".into(), "demo".into()]).unwrap(),
                        SymbolPath::from_names(["CaptureBox"]),
                        SymbolNs::Type,
                    ),
                    explicit_type_args: Vec::new(),
                    fields: vec![TemplateStructField {
                        name: "value".to_string(),
                        value: TemplateExpr::BindingRef(TemplateBindingId(2)),
                    }],
                }),
                params: Vec::new(),
                locals: Vec::new(),
                nested_closures: Vec::new(),
                call_sites: Vec::new(),
                type_sites: Vec::new(),
                iterable_param_slots: Vec::new(),
                references: Vec::new(),
            },
        }),
    };

    let captures = match &closure.kind {
        TemplateDefKind::Closure(template) => template.captures.clone(),
        other => panic!("expected closure template, got {other:?}"),
    };

    assert_eq!(captures, vec![TemplateBindingId(2)]);
    assert!(closure.references().next().is_none());
}

#[test]
fn module_artifact_paths_follow_module_hierarchy() {
    let module_path = ModulePath::new(vec!["std".into(), "format".into(), "csv".into()]).unwrap();
    assert_eq!(
        interface_rel_path(&module_path),
        Path::new("std/format/csv.mci")
    );
    assert_eq!(object_rel_path(&module_path), Path::new("std/format/csv.o"));

    let paths = ModuleArtifactPaths::for_module(Path::new("/tmp/machina-iface"), &module_path);
    assert_eq!(
        paths.interface_path,
        Path::new("/tmp/machina-iface/std/format/csv.mci")
    );
    assert_eq!(
        paths.object_path,
        Path::new("/tmp/machina-iface/std/format/csv.o")
    );
}

#[test]
fn module_interface_from_resolved_context_records_public_export_surface() {
    let resolved = resolved_with_module_path(
        indoc! {r#"
            /// Writes output.
            @public
            fn write(s: string) {}

            @public
            fn map<T>(x: T) -> T { x }

            /// Secret handle.
            @opaque
            @public
            type Secret = { value: u64 }

            @public
            type Conn = {}

            Conn :: {
                /// Ping the connection.
                @public
                fn ping(self) -> u64 { 1 }
            }

            /// Writer contract.
            @public
            trait Writer {
                fn write(self, s: string) -> ();
                prop open: bool { get; }
            }

            type Hidden = {}
        "#},
        "std::demo",
    );

    let interface =
        ModuleInterface::from_resolved_context(&resolved).expect("module path should exist");

    assert_eq!(
        interface.module_path,
        ModulePath::new(vec!["std".into(), "demo".into()]).unwrap()
    );
    assert_eq!(interface.compiler_version, env!("CARGO_PKG_VERSION"));
    assert!(interface.closure_defs.is_empty());

    let export_names = interface
        .exports
        .iter()
        .map(|export| export.symbol_id.to_string())
        .collect::<Vec<_>>();
    assert_eq!(
        export_names,
        vec![
            "std::demo::Conn",
            "std::demo::Conn::ping",
            "std::demo::Secret",
            "std::demo::Writer",
            "std::demo::map",
            "std::demo::write",
        ]
    );

    let write_export = interface
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::write")
        .expect("expected public function export");
    let ExportedDefKind::Func(write) = &write_export.kind else {
        panic!("expected function export");
    };
    assert_eq!(
        write.implementation,
        CallableImplementation::LinkSymbol("write".to_string())
    );
    assert_eq!(write.signature.params.len(), 1);
    assert_eq!(
        write_export
            .tooling
            .as_ref()
            .and_then(|tooling| tooling.doc.as_deref()),
        Some("Writes output.")
    );

    let map_export = interface
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::map")
        .expect("expected generic function export");
    let ExportedDefKind::Func(map) = &map_export.kind else {
        panic!("expected function export");
    };
    let CallableImplementation::GenericTemplate(template) = &map.implementation else {
        panic!("expected emitted generic template");
    };
    assert_eq!(map.signature.type_params, vec!["T"]);
    assert_eq!(map.signature.ret_ty, TypeKey::GenericParam(0));
    assert_eq!(template.root, TemplateDefId(0));
    let root = template
        .root_def()
        .expect("generic template should have root");
    let TemplateDefKind::Function(function) = &root.kind else {
        panic!("expected function template root");
    };
    assert_eq!(
        function.body.expr,
        Some(TemplateExpr::Block {
            items: Vec::new(),
            tail: Some(Box::new(TemplateExpr::BindingRef(TemplateBindingId(0))))
        })
    );
    assert_eq!(function.body.params.len(), 1);
    assert_eq!(function.body.params[0].name, "x");
    assert!(function.body.locals.is_empty());

    let secret_export = interface
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::Secret")
        .expect("expected opaque type export");
    assert_eq!(secret_export.visibility, ExportVisibility::Opaque);
    assert_eq!(
        secret_export
            .tooling
            .as_ref()
            .and_then(|tooling| tooling.doc.as_deref()),
        Some("Secret handle.")
    );

    let method_export = interface
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::Conn::ping")
        .expect("expected public method export");
    let ExportedDefKind::Method(method) = &method_export.kind else {
        panic!("expected method export");
    };
    assert_eq!(method.owner_type, "Conn");
    assert_eq!(
        method.implementation,
        CallableImplementation::LinkSymbol("Conn$ping".to_string())
    );
    assert_eq!(
        method_export
            .tooling
            .as_ref()
            .and_then(|tooling| tooling.doc.as_deref()),
        Some("Ping the connection.")
    );

    let trait_export = interface
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::Writer")
        .expect("expected trait export");
    let ExportedDefKind::Trait(writer) = &trait_export.kind else {
        panic!("expected trait export");
    };
    assert_eq!(writer.methods.len(), 1);
    assert_eq!(writer.properties.len(), 1);
    assert_eq!(writer.properties[0].name, "open");
    assert_eq!(writer.properties[0].ty, TypeKey::Bool);
    assert_eq!(
        trait_export
            .tooling
            .as_ref()
            .and_then(|tooling| tooling.doc.as_deref()),
        Some("Writer contract.")
    );
}

#[test]
fn module_interface_emits_generic_function_struct_literal_template() {
    let resolved = resolved_with_module_path(
        indoc! {r#"
            @public
            type Box<T> = {
                value: T,
            }

            @public
            fn wrap<T>(value: T) -> Box<T> {
                let inner = value;
                Box { value: inner }
            }
        "#},
        "std::demo",
    );

    let interface =
        ModuleInterface::from_resolved_context(&resolved).expect("module path should exist");
    let wrap_export = interface
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::wrap")
        .expect("expected wrap export");
    let ExportedDefKind::Func(wrap) = &wrap_export.kind else {
        panic!("expected function export");
    };
    let CallableImplementation::GenericTemplate(template) = &wrap.implementation else {
        panic!("expected emitted generic template");
    };
    let root = template
        .root_def()
        .expect("generic template should have root");
    let TemplateDefKind::Function(function) = &root.kind else {
        panic!("expected function template root");
    };

    assert_eq!(function.body.params.len(), 1);
    assert_eq!(function.body.locals.len(), 1);
    assert_eq!(function.body.locals[0].name, "inner");
    assert!(function.body.references.iter().any(|reference| {
        matches!(
            reference.target,
            TemplateReferenceTarget::External(ref symbol)
                if symbol.to_string() == "std::demo::Box"
        ) && reference.kind == TemplateReferenceKind::Type
    }));

    let Some(TemplateExpr::Block { items, tail }) = &function.body.expr else {
        panic!("expected block template body");
    };
    assert_eq!(items.len(), 1);
    let Some(tail) = tail else {
        panic!("expected block tail expression");
    };
    let TemplateExpr::StructLit {
        type_symbol,
        explicit_type_args,
        fields,
    } = tail.as_ref()
    else {
        panic!("expected struct literal tail");
    };
    assert_eq!(type_symbol.to_string(), "std::demo::Box");
    assert!(explicit_type_args.is_empty());
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].name, "value");
    assert_eq!(
        fields[0].value,
        TemplateExpr::BindingRef(TemplateBindingId(1))
    );
}

#[test]
fn module_interface_collects_private_generic_closure_dependencies() {
    let resolved = resolved_with_module_path(
        indoc! {r#"
            type Hidden<T> = {
                value: T,
            }

            fn helper<T>(value: T) -> Hidden<T> {
                Hidden { value }
            }

            fn concrete() -> u64 {
                7
            }

            @public
            fn wrap<T>(value: T) -> T {
                let hidden = helper(value);
                let number = concrete();
                value
            }
        "#},
        "std::demo",
    );

    let interface =
        ModuleInterface::from_resolved_context(&resolved).expect("module path should exist");

    let closure_symbols = interface
        .closure_defs
        .iter()
        .map(|closure| closure.symbol_id.to_string())
        .collect::<Vec<_>>();
    assert!(
        closure_symbols
            .iter()
            .any(|symbol| symbol == "std::demo::helper")
    );
    assert!(
        closure_symbols
            .iter()
            .any(|symbol| symbol == "std::demo::concrete")
    );
    assert!(
        closure_symbols
            .iter()
            .any(|symbol| symbol == "std::demo::Hidden")
    );

    let helper = interface
        .closure_defs
        .iter()
        .find(|closure| closure.symbol_id.to_string() == "std::demo::helper")
        .expect("expected helper closure def");
    assert_eq!(
        helper.dependency_kind,
        crate::core::interface::ClosureDependencyKind::CompileTime
    );
    let crate::core::interface::ClosureDefKind::Func(helper_export) = &helper.kind else {
        panic!("expected function closure def");
    };
    assert!(matches!(
        helper_export.implementation,
        CallableImplementation::GenericTemplate(_)
    ));

    let concrete = interface
        .closure_defs
        .iter()
        .find(|closure| closure.symbol_id.to_string() == "std::demo::concrete")
        .expect("expected concrete closure def");
    assert_eq!(
        concrete.dependency_kind,
        crate::core::interface::ClosureDependencyKind::LinkTime
    );
    let crate::core::interface::ClosureDefKind::Func(concrete_export) = &concrete.kind else {
        panic!("expected function closure def");
    };
    assert!(matches!(
        concrete_export.implementation,
        CallableImplementation::LinkSymbol(_)
    ));

    let hidden = interface
        .closure_defs
        .iter()
        .find(|closure| closure.symbol_id.to_string() == "std::demo::Hidden")
        .expect("expected hidden type closure def");
    assert_eq!(
        hidden.dependency_kind,
        crate::core::interface::ClosureDependencyKind::CompileTime
    );
    assert!(matches!(
        hidden.kind,
        crate::core::interface::ClosureDefKind::Type(_)
    ));
}

#[test]
fn module_export_facts_can_be_rebuilt_from_interface_surface() {
    let resolved = resolved_with_module_path(
        indoc! {r#"
            @public
            fn write(s: string) {}

            @public
            fn map<T>(x: T) -> T { x }

            @public
            type Conn = {}

            Conn :: {
                @public
                fn ping(self) -> u64 { 1 }
            }

            @public
            trait Writer {
                fn write(self, s: string) -> ();
            }
        "#},
        "std::demo",
    );

    let interface =
        ModuleInterface::from_resolved_context(&resolved).expect("module path should exist");
    let export_facts = module_export_facts_from_interface(ModuleId(7), &interface);

    assert_eq!(export_facts.module_id.0, 7);
    assert_eq!(
        export_facts.module_path,
        Some(ModulePath::new(vec!["std".into(), "demo".into()]).unwrap())
    );

    let mut callable_names = export_facts.callables.keys().cloned().collect::<Vec<_>>();
    callable_names.sort();
    assert_eq!(callable_names, vec!["map".to_string(), "write".to_string()]);
    assert!(export_facts.types.contains_key("Conn"));
    assert!(export_facts.traits.contains_key("Writer"));
    assert!(export_facts.callables.values().all(|items| {
        items
            .iter()
            .all(|item| item.source.is_none() && item.symbol_id.is_some())
    }));
    assert!(
        export_facts
            .types
            .values()
            .all(|item| item.source.is_none())
    );
    assert!(
        export_facts
            .traits
            .values()
            .all(|item| item.source.is_none())
    );
}

#[test]
fn json_codec_roundtrips_module_interface() {
    let resolved = resolved_with_module_path(
        indoc! {r#"
            /// Writes output.
            @public
            fn write(s: string) {}

            @public
            type Conn = {}
        "#},
        "std::demo",
    );

    let interface =
        ModuleInterface::from_resolved_context(&resolved).expect("module path should exist");
    let bytes = JsonModuleInterfaceCodec::encode(&interface).expect("json encode should succeed");
    let decoded = JsonModuleInterfaceCodec::decode(&bytes).expect("json decode should succeed");

    assert_eq!(decoded, interface);
    let write_export = decoded
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::write")
        .expect("expected public function export");
    assert_eq!(
        write_export
            .tooling
            .as_ref()
            .and_then(|tooling| tooling.doc.as_deref()),
        Some("Writes output.")
    );
}

#[test]
fn emit_module_interface_writes_artifact_at_module_path() {
    let resolved = resolved_with_module_path(
        indoc! {r#"
            @public
            fn write(s: string) {}
        "#},
        "std::demo",
    );
    let run_id = INTERFACE_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let artifact_root = std::env::temp_dir().join(format!(
        "machina_interface_emit_{}_{}",
        std::process::id(),
        run_id
    ));

    let interface_path =
        emit_module_interface_with_codec::<JsonModuleInterfaceCodec>(&artifact_root, &resolved)
            .expect("interface emission should succeed");
    assert_eq!(interface_path, artifact_root.join("std/demo.mci"));

    let decoded = read_module_interface_with_codec::<JsonModuleInterfaceCodec>(&interface_path)
        .expect("interface file should decode");
    assert_eq!(
        decoded
            .exports
            .iter()
            .map(|export| export.symbol_id.to_string())
            .collect::<Vec<_>>(),
        vec!["std::demo::write".to_string()]
    );

    let _ = std::fs::remove_dir_all(&artifact_root);
}

#[test]
fn load_stdlib_module_interface_keeps_real_stdlib_surface() {
    let module_path = ModulePath::new(vec!["std".into(), "io".into()]).unwrap();
    let interface =
        load_stdlib_module_interface_with_codec::<JsonModuleInterfaceCodec>(&module_path)
            .expect("stdlib interface should load");

    assert_eq!(interface.module_path, module_path);
    let export_symbols = interface
        .exports
        .iter()
        .map(|export| export.symbol_id.to_string())
        .collect::<Vec<_>>();
    assert!(export_symbols.iter().any(|name| name == "std::io::print"));
    assert!(export_symbols.iter().any(|name| name == "std::io::IoError"));
    assert!(
        export_symbols
            .iter()
            .all(|name| name.starts_with("std::io::"))
    );
}

#[test]
fn capsule_bindings_can_rebuild_stdlib_exports_from_interface_metadata() {
    let temp_dir = std::env::temp_dir().join(format!(
        "machina_interface_bind_{}_{}",
        std::process::id(),
        INTERFACE_TMP_COUNTER.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&temp_dir).expect("temp dir should be created");
    let entry_path = temp_dir.join("main.mc");
    let entry_source = indoc! {r#"
        requires {
            std::io
        }

        fn main() {
            io::print("hello");
        }
    "#};

    let loader = FsModuleLoader::new(temp_dir.clone());
    let entry_module_path = ModulePath::new(vec!["main".into()]).unwrap();
    let mut capsule = discover_and_parse_capsule_with_loader_and_options(
        entry_source,
        &entry_path,
        entry_module_path,
        &loader,
        CapsuleParseOptions {
            inject_prelude_requires: false,
        },
    )
    .expect("capsule parse should succeed");

    let std_io_id = capsule
        .by_path
        .get(&ModulePath::new(vec!["std".into(), "io".into()]).unwrap())
        .copied()
        .expect("stdlib dependency should be discovered");
    capsule
        .modules
        .get_mut(&std_io_id)
        .expect("stdlib dependency should exist")
        .module
        .top_level_items
        .clear();

    let bindings = CapsuleBindings::build(&CapsuleParsedContext::new(capsule));
    let entry_aliases = bindings.alias_symbols_for(ModuleId(0));
    let io_alias = entry_aliases.get("io").expect("io alias should exist");
    assert!(io_alias.callables.get("print").is_some());
    assert!(io_alias.types.get("IoError").is_some());

    let _ = std::fs::remove_dir_all(&temp_dir);
}
