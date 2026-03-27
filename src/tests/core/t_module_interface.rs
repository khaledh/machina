use crate::core::api::parse_module_with_id_gen;
use crate::core::ast::NodeIdGen;
use crate::core::capsule::{ModuleId, ModulePath};
use crate::core::context::{ParsedContext, module_export_facts_from_interface};
use crate::core::interface::{
    CallableImplementation, ExportVisibility, ExportedDefKind, JsonModuleInterfaceCodec,
    ModuleArtifactPaths, ModuleInterface, ModuleInterfaceCodec, emit_module_interface_with_codec,
    interface_rel_path, object_rel_path, read_module_interface_with_codec,
};
use crate::core::resolve::resolve;
use crate::core::symbol_id::TypeKey;
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
            @public
            fn write(s: string) {}

            @public
            fn map<T>(x: T) -> T { x }

            @opaque
            @public
            type Secret = { value: u64 }

            @public
            type Conn = {}

            Conn :: {
                @public
                fn ping(self) -> u64 { 1 }
            }

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

    let map_export = interface
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::map")
        .expect("expected generic function export");
    let ExportedDefKind::Func(map) = &map_export.kind else {
        panic!("expected function export");
    };
    assert_eq!(
        map.implementation,
        CallableImplementation::GenericBodyPending
    );
    assert_eq!(map.signature.type_params, vec!["T"]);
    assert_eq!(map.signature.ret_ty, TypeKey::GenericParam(0));

    let secret_export = interface
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::Secret")
        .expect("expected opaque type export");
    assert_eq!(secret_export.visibility, ExportVisibility::Opaque);

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
