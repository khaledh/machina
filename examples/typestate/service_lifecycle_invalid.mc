typestate Service {
    fn new() -> Stopped {
        Stopped
    }

    state Stopped {
        fn start() -> Stopped {
            Stopped
        }

        fn start() -> Stopped {
            Stopped
        }
    }
}
