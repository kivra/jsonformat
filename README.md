# jsonformat

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
{"a":"b","gl":"<0.178.0>","level":"info","pid":"<0.182.0>","report_cb":"fun logger:format_otp_report/1","time":1585902924341139}ok
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
