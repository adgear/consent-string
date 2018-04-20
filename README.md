# consent-string

IAB consent-string library

[![Build Status](https://travis-ci.org/adgear/consent-string.svg?branch=master)](https://travis-ci.org/adgear/consent-string?branch=master)

## API

```erlang
1> {ok, Consent} = consent_string:parse_b64(<<"BAAAAAAOYa7j0AcABBENBk-AAAAhGAKAAyAAIABoAIAAcgA0ACYADgAOQBAw">>).
{ok,{consent,1,0,15442098420,28,1,1,<<"EN">>,100,
             <<248,0,0>>,
             529,1,
             {vendor_range,0,10,[25,2,13,32,57,52,76,56,114,259]}}}

2> consent_string:purpose(1, Consent).
true

4> consent_string:purpose([1, 3], Consent).
true

3> consent_string:vendor(32, Consent).
true
```

## Tests

```makefile
make dialyzer
make elvis
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
