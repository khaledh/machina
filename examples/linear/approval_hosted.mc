@linear
type Approval = {
    id: u64,

    states {
        Review,
        Approved,
    }

    actions {
        comment(text: string): Review -> Review,
        approve: Review -> Approved,
    }

    roles {
        Author { comment }
        Reviewer { comment, approve }
    }
}

Approval :: {
    fn comment(self, text: string) -> Review {
        println(text);
        Review {}
    }

    fn approve(self) -> Approved {
        println("approved");
        Approved {}
    }
}

machine ApprovalService hosts Approval(key: id) {
    fn new() -> Self {
        Self {}
    }
}

fn main() -> () | MachineError | SessionError {
    let service = ApprovalService::spawn()?;
    let review = service.create(Approval as Reviewer)?;
    let _approved = review.approve()?;
}
