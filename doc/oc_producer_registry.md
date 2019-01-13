

# Module oc_producer_registry #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Registry maintains a set of metric producers for exporting.

<a name="description"></a>

## Description ##
Most users will rely on the DefaultRegistry.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_producer-1">add_producer/1</a></td><td></td></tr><tr><td valign="top"><a href="#add_producer-2">add_producer/2</a></td><td>
Adds <code>Producer</code> to the <code>Registry</code>.</td></tr><tr><td valign="top"><a href="#read_all-1">read_all/1</a></td><td>Equivalent to <a href="#read_all-1"><tt>read_all(default)</tt></a>.</td></tr><tr><td valign="top"><a href="#read_all-2">read_all/2</a></td><td>
Calls <code>Callback</code> for each metric produced by the metric producers in the <code>Registry</code>.</td></tr><tr><td valign="top"><a href="#read_to_list-0">read_to_list/0</a></td><td></td></tr><tr><td valign="top"><a href="#read_to_list-1">read_to_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_producer-1">remove_producer/1</a></td><td>Equivalent to <a href="#remove_producer-2"><tt>remove_producer(default, Producer)</tt></a>.</td></tr><tr><td valign="top"><a href="#remove_producer-2">remove_producer/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_producer-1"></a>

### add_producer/1 ###

`add_producer(Producer) -> any()`

<a name="add_producer-2"></a>

### add_producer/2 ###

`add_producer(Registry, Producer) -> any()`

Adds `Producer` to the `Registry`.

<a name="read_all-1"></a>

### read_all/1 ###

`read_all(Callback) -> any()`

Equivalent to [`read_all(default)`](#read_all-1).

<a name="read_all-2"></a>

### read_all/2 ###

`read_all(Registry, Callback) -> any()`

Calls `Callback` for each metric produced by the metric producers in the `Registry`.

<a name="read_to_list-0"></a>

### read_to_list/0 ###

`read_to_list() -> any()`

<a name="read_to_list-1"></a>

### read_to_list/1 ###

`read_to_list(Registry) -> any()`

<a name="remove_producer-1"></a>

### remove_producer/1 ###

`remove_producer(Producer) -> any()`

Equivalent to [`remove_producer(default, Producer)`](#remove_producer-2).

<a name="remove_producer-2"></a>

### remove_producer/2 ###

`remove_producer(Registry, Producer) -> any()`

