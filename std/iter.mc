@public
type TryMapIter<S, In, Out, E> = {
    source: S,
    f: fn(In) -> Out | E,
}

TryMapIter<S, In, Out, E> :: {
    @public
    fn iter(self) -> TryMapIter<S, In, Out, E> {
        self
    }

    @public
    fn next(inout self) -> Out | E | IterDone {
        match self.source.next() {
            item: In => {
                let f = self.f;
                return f(item);
            }
            done: IterDone => IterDone {},
            other => other,
        }
    }
}

@public
fn try_map<S, In, Out, E>(
    source: S,
    f: fn(In) -> Out | E,
) -> TryMapIter<S, In, Out, E> {
    TryMapIter { source, f }
}

@public
type MapIter<S, In, Out> = {
    source: S,
    f: fn(In) -> Out,
}

MapIter<S, In, Out> :: {
    @public
    fn iter(self) -> MapIter<S, In, Out> {
        self
    }

    @public
    fn next(inout self) -> Out | IterDone {
        match self.source.next() {
            item: In => {
                let f = self.f;
                f(item)
            }
            done: IterDone => IterDone {},
            other => other,
        }
    }
}

/// Lazily maps each item from `source` through `f`.
@public
fn map<S, In, Out>(source: S, f: fn(In) -> Out) -> MapIter<S, In, Out> {
    MapIter { source, f }
}
