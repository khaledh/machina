# Legacy Typestate Examples

These examples exercise the older `typestate` / protocol surface that Machina is
retiring in favor of `@linear type`.

They remain in the repository temporarily for migration reference and for
checking legacy behavior while the retirement work is still in progress.

If you are learning Machina today, start with the linear examples instead:

- `examples/linear/door.mc`
- `examples/linear/connection.mc`
- `examples/linear/request_builder.mc`
- `examples/linear/approval_hosted.mc`
- `examples/linear/payment_lifecycle.mc`

The default smoke runner (`examples/run_all.sh`) excludes this directory. To
include the legacy examples explicitly:

```bash
MACHINA_INCLUDE_LEGACY_TYPESTATE=1 bash examples/run_all.sh
```
