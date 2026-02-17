typestate FileHandle {
    fn new() -> Closed {
        Closed
    }

    state Closed {
        fn open() -> UnknownState {
            UnknownState
        }
    }
}
