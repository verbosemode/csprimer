# tcp_syn_flood

* Which percentage of syn packets were acked?
...

## Plan
* iterate over frames
* read flags, seqn, ackn
* store seqn in hashtable -> key: seqn, value: frame number
* check hash table on syn/ack for ackn - 1
  - count packet on match

## Details

* Parse [pcap](https://datatracker.ietf.org/doc/html/draft-gharris-opsawg-pcap) file format
  - How are frames delimited?
* relevant TCP header fields
  - flags: 8bit offset 37
    - 0x10 -> ack
    - 0x02 -> syn
  - sequence number: uint32 offset 28
  - acknowledgement number: uint32 offset 32
* syn packet
  - only syn flag set
  - read sequence number
* syn/ack packet
  - syn + ack are set
  - does it ack a previously received syn?
    - ack = syn + 1


## Stretch goals

* Latency to ACK
* ...

## Links

* [Link-Layer Types](https://datatracker.ietf.org/doc/html/draft-ietf-opsawg-pcaplinktype) for PCAP and PCAPNG Capture File Formats
