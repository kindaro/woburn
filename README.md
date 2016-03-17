# woburn

## Introduction

Woburn is a tiling Wayland compositor written in Haskell. Currently it is very
much a work-in-process and is lacking quite a few features, most notably
keyboard and mouse input.

## Design

Woburn is split into three main parts, the frontend, the core and the backend.

### Frontend

The frontend is responible for talking to clients via the Wayland protocol. It
uses the [wayland-wire](https://github.com/sivertb/wayland-wire) library to
talk to the clients. The frontend processes the client requests and pushes them
down to the core.

### Core

The core is where the window layout happens. It provides a request/event
interface for the frontend to speak to. This maps pretty nicely to the Wayland
requests and events, but is somewhat simplified, and it supports multiple
clients (unlike the frontend where each client is kept separate).

Once the core has layed out the windows, it will send a request to the backend
to actually compose them and ouput them to a screen (or whatever else the
backend might be writing to).

### Backend

The backend takes the window layout that it receives from the core, and
composes them onto whatever output it has. Currently there's only one backend,
the Gtk backend. This backend draws the surfaces to a Gtk window. The idea is
to have multiple backends, for example one that uses DRI, one that simply draws
to an image (handy for integration tests maybe?), etc.

In the future the backend will also be responsible for input devices, e.g.
mice and keyboards, and sending input events up to the core.
