

# Module oc_stat_transform #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td>
<code>oc_stat_transform</code> is a parse transform that can detect <code>oc_stat:record</code> calls
with constant measure names and generate remote measure module call from that.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse_transform-2"></a>

### parse_transform/2 ###

`parse_transform(Forms, Options) -> any()`

`oc_stat_transform` is a parse transform that can detect `oc_stat:record` calls
with constant measure names and generate remote measure module call from that.
At the run-time this means we don't have to do a lookup for the module name and
if measure doesn't exist, `{unknown_measure, Name}` error will be thrown.

