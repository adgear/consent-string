-record(consent, {
    version             :: pos_integer(),
    created             :: pos_integer(),
    last_updated        :: pos_integer(),
    cmp_id              :: pos_integer(),
    cmp_version         :: pos_integer(),
    consent_screen      :: pos_integer(),
    consent_language    :: binary(),
    vendor_list_version :: pos_integer(),
    purposes_allowed    :: binary(),
    max_vendor_id       :: non_neg_integer(),
    encoding_type       :: 0..1,
    vendors             :: vendor_bit_field() | vendor_range()
}).

-record(vendor_bit_field, {
    fields :: binary()
}).

-record(vendor_range, {
    default_consent :: 0..1,
    num_entries     :: pos_integer(),
    entries         :: list()
}).

-type consent() :: #consent {}.
-type vendor_bit_field() :: #vendor_bit_field {}.
-type vendor_range() :: #vendor_range {}.
