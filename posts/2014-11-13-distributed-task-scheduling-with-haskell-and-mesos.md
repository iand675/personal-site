---
title: Distributed Task Scheduling with Haskell and Mesos
published: November 13, 2014
---
<div class="bg-gray-900 flex items-center justify-center rounded mb-4"><img alt="Mesos Logo" src="/images/mesos_logo.png"></div>

One of the more intriguing pieces of technology that I've encountered in a while is the [Apache Mesos](http://mesos.apache.org/) project. If you're not familiar with it, it bills itself as a "distributed systems kernel"– an abstraction over "CPU, memory, storage, and other compute resources away from machines (physical or virtual), enabling fault-tolerant and elastic distributed systems to easily be built and run effectively."

To put it in perhaps more motivating terms– Mesos provides you with the mechanisms necessary to better utilize idle computing resources across multiple machines. If you've ever looked at an AWS bill or similar, you can imagine the cost savings that can be gained from using each machine to its fullest capacity.

Mesos initially started off as a research project at Stanford before the researchers were hired on by Twitter to actually make it production-capable; today, the majority of their infrastructure is powered by Mesos under the hood.

All that said, after finding the general concept to be compelling and the code to be heavily used in production at a huge company, I decided that I simply had to have a way to play with it in the ecosystem of my choice: Haskell. So, over the course of several weekends, I wrote some bindings for it which are available on [Hackage](http://hackage.haskell.org/package/hs-mesos).

## Concepts

### Mesos Masters

### Mesos Slaves

### Resource Offers

### Tasks

### The Scheduler

### The Executor

## Let's Build Something!

**Up-front disclaimer:** You'd probably want to use something like ZooKeeper to actually manage scheduler state in a distributed system.
This example is "blog"-sized, and leaves out the usual things you'd want in a bulletproof system. You'll need ZooKeeper to run
the Mesos cluster itself for a real setup, so you might as well use it. Incidentally, there are bindings using the C ZooKeeper library [here on Hackage](http://hackage.haskell.org/package/hzk-2.1.0/docs/Database-Zookeeper.html).

Setting things up:

``` bash
cabal update
cabal sandbox init
cabal install hs-mesos pipes-mesos
```

#### Scheduler.hs

``` haskell
module Main where
import System.Mesos.Scheduler

about = frameworkInfo "" "Beehive"

main = withSchedulerDriver about "127.0.0.1:5050" Nothing go
  where
    go driver = run driver

data BeehiveState = BeehiveState
  { beeCount :: TVar Int
  }

instance ToScheduler BeehiveState where
```

## Now with Pipes!

``` haskell
module Main where
import Pipes.Mesos.Scheduler

```
