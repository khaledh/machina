# Typestate (Legacy Surface)

The old `typestate` keyword is being retired in favor of `@linear type`.

If you are learning Machina today, start with
[Linear Types](linear-types.md) instead.

Why the shift:

- `@linear type` covers direct typestate-style values
- the same declaration grows naturally into hosted workflows with roles,
  triggers, sessions, and `Machine<T>` handles
- it gives Machina one primary stateful-modeling surface instead of splitting
  the story across separate `typestate` and protocol forms

What to use instead:

- direct lifecycle-safe values:
  - `examples/linear/door.mc`
  - `examples/linear/connection.mc`
  - `examples/linear/request_builder.mc`
- hosted workflows:
  - `examples/linear/approval_hosted.mc`
  - `examples/linear/payment_lifecycle.mc`
- narrative overview:
  - `../why-machina.md`

The legacy typestate docs/examples remain useful as implementation history
during the migration, but they are no longer the primary teaching path.
