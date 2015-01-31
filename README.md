This project is a plugin for the build tool SBT.  It is intended to make debugging easier for
forked processes launched by SBT.  It will provide a proxy server for attaching a remote debugger.
This proxy server will hide the restarting for the underlying java process, so you don't have to
reattach your debugger after every restart.

This is still experimental and not fully functioning.
