# Real-time statistics dashboard for Elli

By including `elli_stats` as a middleware you get a nice real-time
overview of requests handled by Elli. You can also provide a grouping
function which allows measurements from calls to unique urls to be
grouped together. Implemented as a middleware, `elli_stats` overrides
the `/elli/stats` URL and displays the dashboard there.

For now it only shows a summary of the requests done in the last
second. This could be extended to also show response codes, sizes,
graphs, etc.

Feedback and pull requests welcome!

## Example

This is an example of `elli_stats` in action:

[Example](doc/elli_stats_example.png)


## Demo

Run `elli_stats:start_demo/0` for a demo. Note: elli is not included
in this project to avoid dependency conflicts, so you need to include
the elli ebin folder on the code path.
