# Changelog

## 0.1.0

Initial release

## Pre release

### August 31, 2022

- Preparing for HEX release
- Added ExDoc config

### August 15, 2022

- Added support for variables, both at compilation and rendering time.
- Added support for custom render functions.
- Changed all render functions to return {ok, Result} or {error, ErrorDetails} despite the fact now we don't return any render error. I did this for compatibility (future releases may return errors).

### August 12, 2022
- The library passed tests and a few websites are running it now.