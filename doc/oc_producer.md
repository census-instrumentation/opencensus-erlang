

# Module oc_producer #
* [Description](#description)

Producer is a source of metrics.

__This module defines the `oc_producer` behaviour.__<br /> Required callback functions: `read/2`.

<a name="description"></a>

## Description ##

Callbacks:

- `read(Registry, Callback) -> ok` - called by exporters.
Read should call `Callback` with the current values of all metrics
supported by this metric provider.
The metrics should be unique for each combination of name and resource.

- `cleanup(Registry) -> ok` - optional.
Called when producer removed from `Registry`
