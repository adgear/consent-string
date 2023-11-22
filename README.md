# consent-string

IAB consent-string library

[![Build Status](https://github.com/adgear/consent-string/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/adgear/consent-string?branch=master)

## API

```erlang
%% for v1
1> consent_string:parse_b64(<<"BAAAAAAOYa7j0AcABBENBk-AAAAhGAKAAyAAIABoAIAAcgA0ACYADgAOQBAw">>).
{ok,{consent,1,0,15442098420,28,1,1,<<"EN">>,100,undefined,
             undefined,undefined,undefined,
             <<248,0,0>>,
             undefined,undefined,undefined,529,1,
             {vendor_range,0,10,[259,114,56|...]},
             undefined,undefined,undefined,undefined,undefined}}

1> {ok, Consent} = consent_string:parse_b64(<<"BAAAAAAOYa7j0AcABBENBk-AAAAhGAKAAyAAIABoAIAAcgA0ACYADgAOQBAw">>).
{ok,{consent,1,0,15442098420,28,1,1,<<"EN">>,100,undefined,
             undefined,undefined,undefined,
             <<248,0,0>>,
             undefined,undefined,undefined,529,1,
             {vendor_range,0,10,[259,114,56|...]},
             undefined,undefined,undefined,undefined,undefined}}

2> consent_string:purpose(1, Consent).
true

3> consent_string:purpose([1, 3], Consent).
true

4> consent_string:vendor(32, Consent).
true

%% same api for v2

1> consent_string:parse_b64(<<"CO4WdepO4WdfWAHABBENA0CsAP_AAH_AAAAAGVtf_X9fb2vj-_5999t0eY1f9_63t-wzjgeNs-8NyZ_X_J4Xr2MyvB34pqYKmR4EulLBAQdkHGHcTQgAwIkVqTLsYk2MizNKJ7JEilMbM2dYGG1vn8XTuZCY70-sf__zv3-_-___6oGUEEmGpfAQJCWMBJNmlUKIEIVxIVAOACihGFo0sNCRwU7I4CPUACABAYgIQIgQYgohZBAAAAAElEQAgAwIBEARAIAAQAjQEIACJAEFgBIGAQACoGhYARRBKBIQYHBUcogQFSLRQTzAAAAA.f_gAD_gAAAAA">>).
{ok,{consent,2,15977797545,15977797590,7,1,1,<<"EN">>,52,2,
     1,0,3072,
     <<255,192,0>>,
     <<127,192,0>>,
     0,<<"AA">>,811,0,
     {vendor_bit_field,<<215,255,95,215,219,...>>},
     {vendor_legitimate_interests,808,
     {entry_bitfield,<<"A&"...>>}},
     {publisher_restrictions,0,[]},
     undefined,undefined,
     {consent_segment,3,...}}}
```

## Tests

```makefile
make dialyzer
make lint
make eunit
make xref
```

## License

```license
The MIT License (MIT)

Copyright (c) 2018 AdGear Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
