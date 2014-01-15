Site#
=========

A simple monitor tool for displaying a graph depicting a URL's performance with rudimentary alerting.


Displays a graph like:

![graph](screenshot.png)

* The taskbar will blink each time the maximum (top dashed line) is exceeded if the window is not focussed.
* Supports tooltips on the graph.
* Can be restarted and paused.
* Settings page allows the url to be set / changed.
* Supports command line parameters (use --help or -h for more information).
* Supports export as CSV.
* Multiple windows can "snap" together and can be moved as a group. Hold the shift key to unsnap a window.

Note that the Release configuration uses ILMerge to create a single executable (Debug doesn't).

License: MIT
