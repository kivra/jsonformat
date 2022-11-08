# jsonformat

A custom formatter for Erlang OTP logger that outputs JSON using `jsx`

## Getting Started

This project should be added as a dependency to your project

### Prerequisites

`sys.config`:

```erlang
[ { kernel
  , [ { logger
      ,[ { handler
         , default
         , logger_std_h
         , #{formatter => {jsonformat, #{new_line => true}}}
         }
       ] }
    , {logger_level, info}
   ] }
].

```

`shell`:

```
> logger:info(#{a => b}).
{"a":"b","gl":"<0.178.0>","level":"info","pid":"<0.182.0>","report_cb":"fun logger:format_otp_report/1","time":1585902924341139}ok
```

### Configuration Options
To print each json object to a new line, set `new_line` to `true`:

```erlang
#{formatter => {jsonformat, #{ new_line => true }}}
```

To specify which kind of line ending to use, set `new_line_type` to
one of `nl`, `crlf`, `cr`, `unix`, `windows` or `macos9`. These
correspond to:

 * `nl` or `unix` means `\n`, ASCII character 0x0A
 * `crlf` or `windows` means `\r\n`, ASCII characters 0x0D 0x0A
 * `cr` or `macos9` means `\r`, ASCII character 0x0D

The default line ending if none is specified is `nl`.

```erlang
#{formatter => {jsonformat, #{ new_line => true, new_line_type => crlf }}}
```

To control what is being included in the log object from the metadata, there
are two ways. One can opt-out from fields. Default opts out is `[report_cb]`.

    #{ meta_without => [report_cb, gl, file, domain] }

Or for very detailed control there is instead opt-in.

    #{ meta_with => [time, mfa, line, user_key, client_key] }

To rename keys in the resulting json object, provide a `key_mapping`. For
example, to rename the `time` and `level` keys to `timestamp` and `lvl`
respectively, use:

```erlang
#{formatter => {jsonformat, #{ key_mapping => #{ time => timestamp
                                               , level => lvl }}}}
```

To format values in the resulting json object, provide a map of `format_funs`.
For example, to format the value associated with the `time` key, use:

```erlang
#{formatter => {jsonformat, #{ format_funs => #{ time => fun(T) -> ... end }}}
```

Note that `key_mapping`s are applied before `format_funs`.

### Built in formatters

    fun jsonformat:system_time_to_iso8601/1

Will take a logger:timestamp() and print it in the format of yyyy-mm-ddTHH:MM:SSZ 

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

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
