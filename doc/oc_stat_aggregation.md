

# Module oc_stat_aggregation #
* [Description](#description)
* [Data Types](#types)

Aggregation represents a data aggregation method.

__This module defines the `oc_stat_aggregation` behaviour.__<br /> Required callback functions: `init/3`, `type/0`, `add_sample/4`, `export/2`, `clear_rows/2`.

<a name="types"></a>

## Data Types ##




### <a name="type-data">data()</a> ###


<pre><code>
data() = <a href="#type-data">data</a>(latest, number()) | <a href="#type-data">data</a>(count, number()) | <a href="#type-data">data</a>(sum, #{count =&gt; non_neg_integer(), mean =&gt; number(), sum =&gt; number()}) | <a href="#type-data">data</a>(distribution, #{count =&gt; non_neg_integer(), mean =&gt; number(), sum =&gt; number(), buckets =&gt; [{number(), non_neg_integer()}]})
</code></pre>




### <a name="type-data">data()</a> ###


<pre><code>
data(Type, AggregationValue) = #{type =&gt; Type, rows =&gt; <a href="#type-data_rows">data_rows</a>(AggregationValue)}
</code></pre>




### <a name="type-data_rows">data_rows()</a> ###


<pre><code>
data_rows(AggregationValue) = [#{tags =&gt; <a href="#type-tv">tv()</a>, value =&gt; AggregationValue}]
</code></pre>




### <a name="type-tv">tv()</a> ###


<pre><code>
tv() = [<a href="oc_tags.md#type-value">oc_tags:value()</a>]
</code></pre>

