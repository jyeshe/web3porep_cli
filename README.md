# porep-cli
Experimental packing CLI to simulate/generate Proof of Replication from 64KiB block data

## Design
The CLI is constructed based on a state machine where every operation is performed without blocking the main process in order to guarantee its responsiveness.

It currently splits the generation of PoCs into two steps: packing the 64KiB and assembling the PoC itself.

                  [wait for command]<----------,
                    /             \            |
               [packbuff]      [genpoc]        |
                /      \        /    \         |
           (64KiB)  (inval) (packed?) (inval)  |
              |         |      |        |      |
          (packing)     '----->|------->'------+
              |                |               |
              '----------------|---------------'
                               |               |
                           (savepoc)-->--------'

## Packing

The packing is performed by a CPU bound NIF that might take on a 4vCPU (4 schedulers) about 20 seconds for the 2000 increments (10ms per call).
A buffer swapping strategy was applied to avoid reallocating the hashing input and output buffers.
For the sake of simplicity, the SHA256 is self contained into the `pack_nif.so` but it can be replaced later with OpenSSL symbol.
  
## DX 

- `make shell`: Allows experimenting calling the NIF.
- `make cli`: Runs the CLI capturing the IO from the App.
- `make cloud`: Emulate cli execution on cloud environment with 4 vCPU.
- `make check`: Runs the test suite.

Using the NIF shell one can get some hashing results by calling:
```
1> poreppack_nif:sha256_hash(crypto:strong_rand_bytes(32), crypto:strong_rand_bytes(32)).
{ok,<<246,247,192,255,42,248,66,214,110,215,167,11,73,
      218,140,83,218,144,51,174,33,218,0,78,143,119,203,
      ...>>}
```

The testing works based on message passing to the state machine, emulating the stdin to avoid prompting during the automated test and asserting the stdout for expected results.

## Usage

For demonstration random miner address, block data and block hash are used as samples but 
there is a `setminer <addr32>` available to set the 32 bytes of the minner address encoded in hex.

With `packbuff <some-block-alias>` it generates a 64KiB buffer. Please notice the block is referenced by an alias to facilitate the demo
and to generate the blockhash randomly as requested. Calling `packbuff` with the same block hash (block alias) generates always the 
same output buffer. This can be verified by the Packing ID. For example:
`packbuff a2b3c4d5` always outputs `Packing ID: 8ef9349e6dc4b527`

After completion a `genpoc` becomes available to obtain the Proof of Custody.

The generation of the PoC was split into two commands to be able to verify that each part is deterministic and to allow async commands in a following version.

## Screenshots
![image](https://github.com/user-attachments/assets/350f8abb-759c-41c1-b90e-a3a8f8dbf24b)
