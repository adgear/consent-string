-define(CONSENT_NOT_ALLOWED_BY_PUBLISHER, 0).
-define(CONSENT_REQUIRE_CONSENT, 1).
-define(CONSENT_REQUIRE_LEGITIMATE_INTEREST, 2).
-define(CONSENT_UNKNOWN, 3).

-record(consent, {
    version                   :: pos_integer(),
    created                   :: pos_integer(),
    last_updated              :: pos_integer(),
    cmp_id                    :: pos_integer(),
    cmp_version               :: pos_integer(),
    consent_screen            :: pos_integer(),
    consent_language          :: binary(),
    vendor_list_version       :: pos_integer(),
    tcf_policy_version        :: undefined | pos_integer(),
    is_service_specific       :: undefined | pos_integer(),
    use_non_standard_stacks   :: undefined | pos_integer(),
    special_feature_optins    :: undefined | pos_integer(),
    purposes_allowed          :: binary(),

    %% li = legitimate interests
    purposes_li_transparency  :: undefined | binary(),
    purposes_one_treatment    :: undefined | pos_integer(),

    %% cc = consent country
    publisher_cc              :: undefined | binary(),

    max_vendor_id             :: non_neg_integer(),
    encoding_type             :: 0..1,
    vendors                   :: undefined | vendor_bit_field() | vendor_range(), %% TODO: refactor this into range_or_bitfield

    vendor_legitimate_interests :: undefined | vendor_legitimate_interests(),
    publisher_restrictions      :: undefined | publisher_restrictions(),

    %% consent segments comaing after the core segment
    disclosed_vendors :: undefined | consent_segment(),
    allowed_vendors   :: undefined | consent_segment(),
    publisher_tc      :: undefined | consent_segment()
}).

-record(addtl_consent, {
    version :: pos_integer(),
    atp_ids :: [pos_integer()],
    %% Only version 2 has this field potentially set
    disclosed_atp_ids :: undefined | [pos_integer()]
}).

-record(consent_segment_entry_disclosed_vendors, {
    max_vendor_id :: pos_integer(),
    entries       :: range_or_bitfield()
}).

-record(consent_segment_entry_allowed_vendors, {
    max_vendor_id :: pos_integer(),
    entries       :: range_or_bitfield()
}).

-record(consent_segment_entry_publisher_purposes, {
    pub_purposes_consent         :: binary(),
    pub_purposes_li_transparency :: binary(),
    num_custom_purposes          :: pos_integer(),
    custom_purposes_consent      :: binary(),
    custom_purposes_li           :: binary()
}).

-record(consent_segment, {
    type    :: consent_segment_type(),
    segment :: consent_segment_entry()
}).

-record(entry_range, {
    num_entries :: pos_integer(),
    entries     :: list()
}).

-record(entry_bitfield, {
    fields :: binary()
}).

-record(vendor_bit_field, {
    fields :: binary()
}).

-record(vendor_range, {
    default_consent :: undefined | 0..1,
    num_entries     :: pos_integer(),
    entries         :: list()
}).

-record(vendor_legitimate_interests, {
    max_vendor_id :: pos_integer(),
    interests     :: range_or_bitfield()
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
-type addtl_consent() :: #addtl_consent {}.

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

-type restriction_not_allowed_by_publisher()    :: ?CONSENT_NOT_ALLOWED_BY_PUBLISHER.
-type restriction_require_consent()             :: ?CONSENT_REQUIRE_CONSENT.
-type restriction_require_legitimate_interest() :: ?CONSENT_REQUIRE_LEGITIMATE_INTEREST.
-type restriction_unknown()                     :: ?CONSENT_UNKNOWN.

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
