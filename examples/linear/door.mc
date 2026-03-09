@linear
type Door = {
    states {
        Closed,
        Open,
    }

    actions {
        open: Closed -> Open,
        close: Open -> Closed,
    }
}

Door :: {
    fn open(self) -> Open {
        println("opening");
        Open {}
    }

    fn close(self) -> Closed {
        println("closing");
        Closed {}
    }
}

fn main() {
    let door = Door::Closed {};
    let door = door.open();
    let _door = door.close();
}
