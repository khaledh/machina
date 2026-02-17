typestate RequestBuilder {
    fields {
        url: string,
    }

    fn new(url: string) -> Empty {
        Empty { url: url }
    }

    state Empty {
        fields {
            url: string,
        }
    }
}
