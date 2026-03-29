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
    CallableImplementation, ExportVisibility, ExportedDefKind, JsonModuleInterfaceCodec,
    ModuleArtifactPaths, ModuleInterface, ModuleInterfaceCodec, emit_module_interface_with_codec,
    interface_rel_path, load_stdlib_module_interface_with_codec, object_rel_path,
    read_module_interface_with_codec,
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
fn json_codec_roundtrips_interface_param_default_flags() {
    let resolved = resolved_with_module_path(
        indoc! {r#"
            @public
            fn connect(host: string, port: u64 = 443, timeout: u64 = 30) -> u64 { port + timeout }

            @public
            type Client = {}

            Client :: {
                @public
                fn request(self, path: string, method: string = "GET") -> string { path }
            }
        "#},
        "std::demo",
    );

    let interface =
        ModuleInterface::from_resolved_context(&resolved).expect("module path should exist");
    let encoded = JsonModuleInterfaceCodec::encode(&interface).expect("interface should encode");
    let decoded = JsonModuleInterfaceCodec::decode(&encoded).expect("interface should decode");

    let connect = decoded
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::connect")
        .expect("expected connect export");
    let ExportedDefKind::Func(connect) = &connect.kind else {
        panic!("expected function export");
    };
    assert_eq!(
        connect
            .signature
            .params
            .iter()
            .map(|param| param.has_default)
            .collect::<Vec<_>>(),
        vec![false, true, true]
    );

    let request = decoded
        .exports
        .iter()
        .find(|export| export.symbol_id.to_string() == "std::demo::Client::request")
        .expect("expected request method export");
    let ExportedDefKind::Method(request) = &request.kind else {
        panic!("expected method export");
    };
    assert_eq!(
        request
            .signature
            .params
            .iter()
            .map(|param| param.has_default)
            .collect::<Vec<_>>(),
        vec![false, true]
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
