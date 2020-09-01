-record(consent, {
    version                   :: pos_integer(),
    created                   :: pos_integer(),
    last_updated              :: pos_integer(),
    cmp_id                    :: pos_integer(),
    cmp_version               :: pos_integer(),
    consent_screen            :: pos_integer(),
    consent_language          :: binary(),
    vendor_list_version       :: pos_integer(),
    tcf_policy_version        :: pos_integer(),
    is_service_specific       :: pos_integer(),
    use_non_standard_stacks   :: pos_integer(),
    special_feature_optins    :: pos_integer(),
    purposes_allowed          :: binary(),

    %% li = legitimate interests
    purposes_li_transparency  :: binary(),
    purposes_one_treatment    :: pos_integer(),

    %% cc = consent country
    publisher_cc              :: pos_integer(),

    max_vendor_id             :: non_neg_integer(),
    encoding_type             :: 0..1,
    vendors                   :: vendor_bit_field() | vendor_range(), %% TODO: refactor this into range_or_bitfield

    vendor_legitimate_interests :: vendor_legitimate_interests(),
    publisher_restrictions      :: publisher_restrictions(),

    segments :: list(consent_segment())
}).

-record(consent_segment_entry_disclosed_vendors, {
    max_vendor_id :: pos_integer(),
    entries       :: range_or_bitfield()
}).

-record(consent_segment_entry_allowed_vendors, {
}).

-record(consent_segment_entry_publisher_purposes, {
}).

-record(consent_segment, {
    type  :: consent_segment_type(),
    entry :: consent_segment_entry()
}).

-record(entry_range, {
    num_entries :: pos_integer(),
    entries     :: list()
}).

-record(entry_bitfield, {
    field :: binary()
}).

-record(vendor_bit_field, {
    fields :: binary()
}).

-record(vendor_range, {
    default_consent :: undefined | 0..1,
    num_entries     :: pos_integer(),
    entries         :: list()
}).

-record(vendor_legitimate_interests_range, {
    num_entries :: pos_integer(),
    entries     :: list()
}).

-record(vendor_legitimate_interests_entry, {
    fields :: binary()
}).

-record(vendor_legitimate_interests, {
    max_vendor_id :: pos_integer(),
    interests     :: vendor_legitimate_interests_range() |
                     vendor_legitimate_interests_entry()
}).

-record(publisher_restrictions_entry, {
    restriction_type :: restriction_type(),
    purpose_id       :: pos_integer(),
    num_entries      :: pos_integer(),
    entries          :: list()
}).

-record(publisher_restrictions, {
    num_pub_restrictions :: non_neg_integer(),
    entries              :: list(publisher_restrictions_entry())
}).

-type consent() :: #consent {}.

-type consent_segment() :: #consent_segment {}.

-type consent_segment_default() :: 0.
-type consent_segment_disclosed_vendors() :: 1.
-type consent_segment_allowed_vendors() :: 2.
-type consent_segment_publisher_tc() :: 3.

-type consent_segment_type() :: consent_segment_default() |
                                consent_segment_disclosed_vendors() |
                                consent_segment_allowed_vendors() |
                                consent_segment_publisher_tc().

-type consent_segment_entry() :: consent_segment_entry_disclosed_vendors() |
                                 consent_segment_entry_allowed_vendors() |
                                 consent_segment_entry_publisher_purposes().

-type publisher_restrictions()       :: #publisher_restrictions {}.
-type publisher_restrictions_entry() :: #publisher_restrictions_entry {}.

-type entry_range() :: #entry_range {}.
-type entry_bitfield() :: #entry_bitfield {}.
-type range_or_bitfield() :: entry_range() | entry_bitfield().

-type restriction_not_allowed_by_publisher() :: 0.
-type restriction_require_consent() :: 1.
-type restriction_require_legitimate_interest() :: 2.
-type restriction_unknown() :: 3.

-type restriction_type() :: restriction_not_allowed_by_publisher() |
                            restriction_require_consent() |
                            restriction_require_legitimate_interest() |
                            restriction_unknown().

-type consent_segment_entry_disclosed_vendors()  :: #consent_segment_entry_disclosed_vendors {}.
-type consent_segment_entry_allowed_vendors()    :: #consent_segment_entry_allowed_vendors {}.
-type consent_segment_entry_publisher_purposes() :: #consent_segment_entry_publisher_purposes {}.

-type vendor_bit_field() :: #vendor_bit_field {}.
-type vendor_range() :: #vendor_range {}.

-type vendor_legitimate_interests()       :: #vendor_legitimate_interests {}.
-type vendor_legitimate_interests_range() :: #vendor_legitimate_interests_range {}.
-type vendor_legitimate_interests_entry() :: #vendor_legitimate_interests_entry {}.
