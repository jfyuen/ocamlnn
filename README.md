# OCaml NN

This is a very old project done at the uni available here for archive.

A simple neural network in OCaml, with an image encoder and decoder.

## Prerequisites

OCaml of course!

## Installation

For byte code (slow):
```sh
$ make bc
```

For native code (much faster):
```sh
$ make nc
```

Clean up:
```sh
$ make clean
```

## Usage

2 binaries are created: an `encoder` in `encoder/` and a `decoder` in `decoder/`

### Encoder

The encoder takes a neural network configuration file, here located in `encoder/mlp.conf` and an image in pgm format located in `encoder/images`. Learning stops at 1000 iterations in the default `mlp.conf` or if the error is small enough. Check `encoder/mlp.conf` for more options.

```sh
$ ./encoder -f mlp.conf -d encoder.nn -o compressed_image.img images/lena.pgm
```

### Decoder

Decodes a compressed image with a neural network definition previousl saved by encoder.

```sh
$ ./decoder -d encoder.nn -i compressed_image.img -o output.pgm
```

