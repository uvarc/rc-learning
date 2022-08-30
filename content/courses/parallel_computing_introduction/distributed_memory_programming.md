---
title: "Distributed-Memory Programming"
toc: true
type: docs
weight: 20
menu:
    parallel_programming:
        weight: 20
---

The model is  _nodes_ (computing systems) connected by an  _interconnection network_.  Each node consist of processors, memory, and network.  Some form of disk storage is usually available, though it is not always local to the node but could be shared among all the nodes.

{{< diagram >}}
flowchart LR
   subgraph Users
       A(User) --> F((Internet))
       B(User) --> F
       C(User) --> F
    end
    subgraph Cluster
       F --> G(Frontend)
       G --> H{{Interconnection Network}}
       H --> K(Node)
       H --> L(Node)
       H --> M(Node)
       H --> N(Node)
       H --> S[(Storage)]
    end
{{< /diagram >}}

In distributed-memory programming, each node runs some number of _independent_ and identical copies of the executable. This means that if the program runs on $N$ nodes, with $M$ copies per node, the total number of processes is $N \times M$.

Since these processes do not have the ability to access any of another team member's physical memory, they must communicate using **messages**.  A message consists of a stream of bytes, with some form of address format to be sure the message is received at the right target at the right time. 

Communication by messages means that a byte stream is transmitted from Process $M_i$, the _sender_, to  Process $M_j$, the _receiver_. The message "address" could be formed from information including
- IP address and port pairs
- Logical task number
- Process ID

The software that implements message passing is usually implemented as a library.  Programmers use the application programming interface (API) to manage interprocess communication.  Most libraries will use internal networks among processes running on the same node, but must use the external interconnection network for internode communications.  Therefore, network bandwith and especially latency are important for distributed parallel computing.

