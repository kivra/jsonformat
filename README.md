# Project Title

A custom formatter for Erlang OTP logger that outputs JSON using `jsx`

## Getting Started

This project should be added as a dependency to your project

### Prerequisites

`sys.config`:

```erlang
[ { kernel
  , {logger_level, info}
  , [ { logger
      ,[ { handler
         , default
         , logger_std_h
         , #{formatter => {jsonformat, #{}}}
         }
       ] }
   ] }
].

```

`shell`:

```
> logger:info(#{a => b}).
{"a":"b","gl":"�gd\u0000\rnonode@nohost\u0000\u0000\u0000�\u0000\u0000\u0000\u0000\u0000","level":"info","pid":"�gd\u0000\rnonode@nohost\u0000\u0000\u0000�\u0000\u0000\u0000\u0000\u0000","report_cb":"�qd\u0000\u0006loggerd\u0000\u0011format_otp_reporta\u0001","time":1585900670785792}ok
until finished
```

## Running the tests

```
$ rebar3 eunit
```

### Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/kivra/jsonformat/tags).

### Commit message convention
We use [Conventional Commits](https://www.conventionalcommits.org).

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
