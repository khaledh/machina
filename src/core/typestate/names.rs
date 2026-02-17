//! Shared typestate desugaring symbol names.
//!
//! Keeping these names in one module makes `mod.rs` orchestration easier to
//! read and keeps generated-language surface identifiers centralized.

pub(super) const MACHINE_HANDLE_TYPE_NAME: &str = "Machine";
pub(super) const MACHINE_ERROR_TYPE_NAME: &str = "MachineError";

pub(super) const MACHINE_SPAWN_FAILED_TYPE_NAME: &str = "MachineSpawnFailed";
pub(super) const MACHINE_BIND_FAILED_TYPE_NAME: &str = "MachineBindFailed";
pub(super) const MACHINE_START_FAILED_TYPE_NAME: &str = "MachineStartFailed";
pub(super) const MACHINE_UNKNOWN_TYPE_NAME: &str = "MachineUnknown";
pub(super) const MACHINE_NOT_RUNNING_TYPE_NAME: &str = "MachineNotRunning";
pub(super) const MACHINE_MAILBOX_FULL_TYPE_NAME: &str = "MailboxFull";
pub(super) const MACHINE_REQUEST_FAILED_TYPE_NAME: &str = "RequestFailed";
pub(super) const MACHINE_MANAGED_RUNTIME_UNAVAILABLE_TYPE_NAME: &str = "ManagedRuntimeUnavailable";

pub(super) const MACHINE_TARGET_ID_HELPER_FN: &str = "__mc_machine_target_id";
pub(super) const MANAGED_RUNTIME_DEFAULT_MAILBOX_CAP: u64 = 8;
pub(super) const MANAGED_RUNTIME_BOOTSTRAP_FN: &str = "__mc_machine_runtime_managed_bootstrap_u64";
pub(super) const MANAGED_RUNTIME_CURRENT_FN: &str = "__mc_machine_runtime_managed_current_u64";
pub(super) const MANAGED_RUNTIME_SHUTDOWN_FN: &str = "__mc_machine_runtime_managed_shutdown_u64";
pub(super) const MANAGED_RUNTIME_STEP_FN: &str = "__mc_machine_runtime_step_u64";

pub(super) const MACHINE_ERROR_VARIANT_SPAWN_FAILED: &str = "SpawnFailed";
pub(super) const MACHINE_ERROR_VARIANT_BIND_FAILED: &str = "BindFailed";
pub(super) const MACHINE_ERROR_VARIANT_START_FAILED: &str = "StartFailed";
pub(super) const MACHINE_ERROR_VARIANT_MANAGED_RUNTIME_UNAVAILABLE: &str = "RuntimeUnavailable";
pub(super) const MACHINE_ERROR_VARIANT_UNKNOWN: &str = "Unknown";
pub(super) const MACHINE_ERROR_VARIANT_NOT_RUNNING: &str = "NotRunning";
pub(super) const MACHINE_ERROR_VARIANT_MAILBOX_FULL: &str = "MailboxFull";
pub(super) const MACHINE_ERROR_VARIANT_REQUEST_FAILED: &str = "RequestFailed";
