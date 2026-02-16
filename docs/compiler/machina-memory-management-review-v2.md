# CVS Design Review (v2)

Review of the Confined Value Semantics design proposal
(`machina-memory-management-design.md`) and its relationship to the current
memory safety model (`docs/guide/mem-safety.md`).

February 2026.

---

## Overall Assessment

The design is directionally strong. It preserves the "explicit ownership + value
semantics" core while adding practical tools for performance (COW),
expressiveness (regions), and actor isolation (per-machine heaps). The layered
progressive disclosure is well-structured.

The main concern is not intent — it is specification precision. Several rules are
underspecified or internally inconsistent at the Layer 1/3/4 boundaries, and
those are exactly where soundness bugs tend to hide. The clear recommendation is
to formalize a single provenance/ownership model before implementation proceeds
further on regions and transfer.

---

## 1. Internal Consistency

The two documents are mostly compatible, but there are several tensions:

- **"Independent copies" vs. shared backing.** The user guide says "assignment
  produces independent copies." Layer 1 says "assignment shares backing storage
  and increments a reference count." These are only consistent if backing
  identity is strictly unobservable in safe code. This invariant should be stated
  explicitly.

- **`nocopy` self-contradiction.** The formal semantics table says COW handle
  sharing (refcount increment) is "not a copy for `nocopy` purposes," but COW
  detach on mutation *is* a copy. This creates a situation where `let b = a` is
  allowed for a `nocopy` value (it's just handle sharing), but then `b.append(x)`
  triggers a compile error (detach is a copy). The rules need one unambiguous
  formulation — either `nocopy` is affine (forbids alias-creating assignment
  entirely), or it is uniqueness-gated (allows sharing but makes
  detach-requiring mutation a compile error unless uniqueness is proven).

- **`T^` semantic drift.** Document 1 frames `T^` as "heap-owned value."
  Document 2 makes `^expr` allocator-dependent: inside a region, it allocates
  into the region rather than the heap. This is a real model shift — the same
  syntax produces values with different lifetimes and provenances. This should be
  called out explicitly, and the terminology should be updated to reflect that
  `T^` means "owned, dynamically allocated value" rather than specifically
  "heap-allocated."

- **Layer 4 payload eligibility.** Described as a structural type check, but the
  examples require value provenance checks (region-origin). A type-only check is
  insufficient — `u64[*]` is a payload-eligible type, but a `u64[*]` allocated
  inside a region is not payload-eligible as a value. Provenance must be tracked.

- **`inout` and COW.** Layer 1 says `inout` has "no COW interaction (there is no
  second handle)," but second handles can exist after prior assignment. If `a` is
  a COW collection and `let b = a` was executed earlier, then `tweak(inout a)`
  triggers COW detach. The interaction exists and should be specified.

---

## 2. Completeness Gaps

Several important areas are not addressed:

- **No formal provenance definition.** Many rules implicitly depend on
  distinguishing heap-owned, region-owned, shared-frozen, and borrowed values.
  There is no formal definition of these categories or their relationships.

- **Region teardown and drop semantics.** Does bulk free run element destructors,
  and in what order? This matters for non-memory resources (file handles, network
  connections) and for correctness with nested owned values. If region teardown
  does not run drops, that must be a stated restriction (region-allocated values
  must not hold non-memory resources). If it does run drops, the ordering and
  failure semantics must be specified.

- **Liveness algorithm for closures and iterators.** The proposal says closures
  and iterators "participate in liveness" but does not specify the algorithm.
  Edge cases include: closures in loops, conditionals, early returns, nested
  captures, closures passed to higher-order functions, and closures stored in
  region-allocated structs.

- **Generic constraints for new predicates.** `PayloadEligible`, `FreezeEligible`,
  and `Copyable`/`NoCopy` interactions need trait-like constraints for generic
  code. Without them, library authors cannot write functions generic over types
  that may or may not be payload-eligible or freeze-eligible.

- **Direct-mode concurrency assumptions.** Non-atomic COW refcounts are safe only
  under strict non-sharing guarantees. The document assumes single-threaded
  machines, but if machines execute concurrently on different threads (with
  isolated heaps), any shared backing outside `shared` must be impossible by
  construction. This invariant should be stated and verified.

- **FFI contract.** What happens when a region reference or shared value crosses
  an FFI boundary? What is forbidden, what requires `unsafe`, and what runtime
  checks exist?

---

## 3. Soundness Concerns

### COW + send pointer handoff

If a moved payload still shares COW backing storage with another local alias,
the O(1) pointer handoff optimization creates cross-machine shared mutable state
and refcount races. Example:

```
let a: u64[*] = [1, 2, 3];
let b = a;                    // COW: shared backing, refcount = 2
send(target, move a);         // pointer handoff: target now shares backing with b
b.append(4);                  // COW detach in sender — but refcount is now
                              // split across two machines with non-atomic RC
```

The transfer path must either require uniqueness (refcount == 1) or force a
detach/deep-copy before transfer. This must be specified.

### Region escape via provenance erasure

If eligibility and escape checks are purely type-based, a region-origin value of
an eligible type can leak. For example, a `u64[*]` allocated in a region is
structurally identical to one allocated on the heap, but returning it from the
region would create a dangling reference. Provenance must be tracked in the IR
or type context, not just the surface type.

### Region teardown without drops

If region bulk-free does not run destructors, then region-allocated values
containing owned heap values (`T^` fields pointing outside the region) will
leak. If it does run destructors, the execution order and failure handling must
be specified to avoid double-drops in nested structures.

### `nocopy` + runtime COW detach

Making `nocopy` sound at compile time requires either prohibiting COW handle
sharing entirely (affine) or proving uniqueness before any mutation that would
trigger detach. The current specification allows sharing but rejects detach,
which means a `nocopy` COW collection can be shared but never mutated through
any handle — an odd and potentially confusing state.

### Atomicity model

Single-threaded handlers do not imply a single-threaded runtime. If machines
execute concurrently on different OS threads, any shared backing storage outside
of `shared` (which uses atomic refcounts) must be impossible by construction.
The COW refcount being non-atomic is safe only if the backing storage is truly
confined to one machine's heap.

---

## 4. Design Tension Points

- **COW vs. "no hidden aliasing."** COW introduces hidden sharing of backing
  storage. This is acceptable if identity is unobservable, but it pushes against
  the teaching model ("changing one value does not secretly change another").
  The distinction between "value independence" and "storage independence" should
  be made explicit in documentation.

- **First-class vs. second-class references.** Regions add first-class references
  while Layer 2 remains second-class. This duality is workable but needs very
  crisp boundaries. Users must understand exactly where references become
  first-class (inside a `region` block) and where they remain second-class
  (everywhere else).

- **Ambient allocator context-sensitivity.** Identical `^expr` code has different
  lifetime and provenance semantics inside vs. outside a `region`. This is
  elegant but means that moving code into or out of a region block can silently
  change allocation behavior. Refactoring hazard.

- **Transfer semantics vs. runtime optimization freedom.** Pointer handoff,
  serialization, and memcpy are not equivalent unless uniqueness and provenance
  constraints are guaranteed at the language level. The runtime cannot freely
  choose strategies without compiler cooperation.

- **`shared T` and read-only parameter interop.** Accepting both `T` and
  `shared T` in read-only parameters is ergonomic, but the copy/move behavior
  around APIs that return owned `T` from borrowed/shared inputs needs explicit
  rules to avoid confusion.

---

## 5. Practical Concerns

- **Conservative overlap checks.** Already restrictive for non-constant indices.
  With regions and iterators adding more aliasing scenarios, this can become a
  common friction point. Consider a roadmap for smarter overlap analysis (e.g.,
  provably-disjoint index ranges).

- **No region polymorphism in V1.** Reusable libraries that operate on
  region-allocated data will be awkward to write. Region-aware helpers may become
  monolithic or duplicated. This is acceptable for V1 but the roadmap should be
  concrete.

- **Mandatory copy-out from regions.** For large results, the copy at the region
  boundary is unavoidable. Users may over-widen regions to avoid copies,
  increasing memory pressure and reducing the benefits of scoped deallocation.

- **Transitive `nocopy` contagion.** Similar to Rust's `!Send`/`!Sync`
  propagation, `nocopy` can become viral through struct containment and API
  boundaries. This is manageable but should be documented as intentional, with
  guidance on when to use it and when to restructure with `sink`/move instead.

- **Diagnostic quality.** Progressive disclosure only works if diagnostics are
  excellent. Error messages must explain "borrow source," "region boundary," and
  "eligibility failure path" clearly. The diagnostic families table is a good
  start.

---

## 6. Comparison with Prior Art

| System | Relevant lesson | CVS relationship |
|--------|-----------------|------------------|
| **Rust** | Trait-based capability predicates (`Send`, `Sync`) | CVS should adopt similar constraints for payload/freeze eligibility in generics |
| **Swift** | COW works in production, but paired with strict exclusivity enforcement | CVS needs equivalent mutation-side guarantees for COW soundness |
| **Pony** | Capability-based actor isolation prevents accidental sharing by type | CVS could adopt lightweight capability markers rather than ad hoc eligibility lists |
| **Erlang/BEAM** | Copy-on-send is semantically simple and robust | Pointer-handoff optimization is fine only under proven uniqueness |
| **Cyclone/MLKit** | Regions are powerful but library ergonomics suffer without polymorphism | V1 limitation is acceptable, but roadmap should be concrete |
| **Hylo/Val** | Baseline is aligned, but boundary semantics must be mechanically precise | Risk of "performance surprise language" if copy points are not predictable |

---

## 7. Naming and Terminology

| Current term | Issue | Suggestion |
|--------------|-------|------------|
| `T^` ("heap value") | Misleading once `^` targets regions | Use neutral ownership terminology ("owned allocated value") |
| "Second-class references" | Technically correct but user-hostile | Consider "scoped borrows" for documentation |
| `nocopy` | Overloaded: implicit copy prohibition vs. COW detach behavior | Clarify semantics first, then evaluate if name fits |
| `shared` | Reads as generic sharing; semantics are specifically frozen immutability | Consider `frozen` to communicate the immutability invariant |
| "Payload-eligible" / "freeze-eligible" | Good terms | Need explicit distinction between type-structural and value-provenance checks |

---

## 8. Recommended Actions

### High priority

1. **Write a formal provenance lattice.** Define ownership categories
   (`Owned<MachineHeap>`, `Owned<Region r>`, `Borrowed<scope>`, `SharedFrozen`)
   and base all eligibility checks on this model. This is the foundation
   everything else depends on.

2. **Reconcile Layer 1 wording.** Pin down `nocopy` semantics (affine vs.
   uniqueness-gated), `inout` + COW interaction, and the assignment
   observability invariant. Remove contradictions.

3. **Specify transfer preconditions.** Especially uniqueness requirements for the
   pointer-handoff optimization. The compiler must guarantee that transferred
   values have refcount == 1 or force a detach before transfer.

### Medium priority

4. **Add region teardown and drop semantics.** Specify whether destructors run,
   in what order, and what happens on failure. This affects whether
   region-allocated values can hold non-memory resources.

5. **Add generic capability constraints.** Even if syntax is provisional, define
   how generic code interacts with `PayloadEligible`, `FreezeEligible`, and
   `NoCopy`. Library design will stall without this.

6. **Build a memory model litmus test suite.** Executable compiler tests for
   COW/send aliasing, region escape, closure capture, rollback staging, and
   provenance tracking. Lock behavior with conformance tests tied to each
   diagnostic family.

### Lower priority

7. **Roadmap for region polymorphism.** Even a sketch of the future syntax and
   semantics would help library authors plan.

8. **Smarter overlap analysis.** Provably-disjoint index ranges, NLL-style
   borrow end, and other precision improvements to reduce false positives.

---

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Optimization-driven aliasing breaks isolation/safety | Require uniqueness proofs + debug assertions on transfer paths |
| Ergonomics regress due to conservative rules | Roadmap for precision improvements (NLL-style borrow end, smarter overlap analysis) |
| Spec/implementation drift | Lock behavior with conformance tests tied to each diagnostic family |
| `nocopy` contagion frustrates users | Document as intentional; provide guidance on `sink`/move alternatives |
| Region copy-out cost encourages over-wide regions | Provide scoping guidance and tooling to surface region memory pressure |
