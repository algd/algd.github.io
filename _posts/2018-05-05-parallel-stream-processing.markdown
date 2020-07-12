---
layout: post
title:  "Parallel stream processing with Akka Streams"
date:   2018-08-05 20:36:40
image:  "/images/20180805/icon.png"
header: "/images/20180805/header.jpg"
level: Intermediate
categories: "Akka Streams"
---

{% include custom/20180805/streams.html %}



The default behavior for Akka Streams is processing sequentially every item coming  from the stream,
which usually is the safest but not the most efficient way.
It depends on the logic you are implementing but, even for simple streams, we might want to change this.

_Note: there is a visual demo right before every execution output that illustrates how elements in the stream are being
processed. If you want to play it this you need to have JavaScript enabled._

In order to explain the different types of parallelism, let's create a pipeline with some actions so it's easier to think about it.
Let's define a function that can define a task with some specific duration:
{% highlight scala %}
def task(name: String, duration: Int)(item: String): String = {
  println(s"Starting [$name] for [item $item]")
  Thread.sleep(duration)
  println(s"Finishing [$name] for [item $item]")
  item
}
{% endhighlight %}

Then, for example, we can create a stream that represents the following:

- Receive an infinite stream where each element is the url of a text file.
- Download the text file given the url (~2 seconds).
- Count the number of words in the file (~0.1 seconds).
- Open a file, update the number and close the file (this is not the best practice but we need an excuse to keep this step sequential) (~1 second).


{% highlight scala %}
val fileUrls = Source.repeat(()).zipWithIndex.map(i => (i._2 + 1).toString) // "1", "2", "3" ...
val downloadFile = Flow[String].map(task("Download file", 2000)) // long task
val countWords = Flow[String].map(task("Count words", 100)) // short task
val updateFile = Flow[String].map(task("Update file", 1000)) // long task

fileUrls
  .via(downloadFile)
  .via(countWords)
  .via(updateFile)
  .runWith(Sink.ignore)
{% endhighlight %}

Then, if we run this, we have:

{% include custom/20180805/graph1.html %}
{% highlight markdown %}
Starting [Download file] for [item 1]
Finishing [Download file] for [item 1]
Starting [Count words] for [item 1]
Finishing [Count words] for [item 1]
Starting [Update file] for [item 1]
Finishing [Update file] for [item 1]
Starting [Download file] for [item 2]
Finishing [Download file] for [item 2]
Starting [Count words] for [item 2]
Finishing [Count words] for [item 2]
Starting [Update file] for [item 2]
Finishing [Update file] for [item 2]
Starting [Download file] for [item 3]
...
{% endhighlight %}

## Horizontal parallelism

This type of parallelism refers to executing the same code in parallel for different data. From the previous steps, we can parallelise the file download and the word count. We can achieve this in different ways.

#### MapAsync
Usually when we work with this kind of tasks they are wrapped in a future, so we can use `mapAsync`. Let's say we want to download two files simultaneously, then we set the parameter `parallelism` to 2:
{% highlight scala %}
val downloadFileAsync = Flow[String].mapAsync(parallelism = 2){ item =>
  Future(task("Download file", 2000)(item))
}
{% endhighlight %}

If we replace this flow in the previous graph:
{% highlight scala %}
fileUrls
  .via(downloadFileAsync)
  .via(countWords)
  .via(updateFile)
  .runWith(Sink.ignore)
{% endhighlight %}

We get:

{% include custom/20180805/graph2.html %}

{% highlight markdown %}
Starting [Download file] for [item 1]
Starting [Download file] for [item 2]
Finishing [Download file] for [item 1]
Finishing [Download file] for [item 2]
Starting [Count words] for [item 1]
Finishing [Count words] for [item 1]
Starting [Update file] for [item 1]
Finishing [Update file] for [item 1]
Starting [Count words] for [item 2]
Finishing [Count words] for [item 2]
Starting [Update file] for [item 2]
Finishing [Update file] for [item 2]
Starting [Download file] for [item 3]
Starting [Download file] for [item 4]
...
{% endhighlight %}

This looks better, processing two files is taking 2 seconds less than before, because when we start processing the second file it is already downloaded.
We could also count the words in parallel but the thing that looks worst here is that we have to wait until we have updated the output file to start downloading the next file.

#### Custom graph
If you don't want to use Future here you can implement a custom graph as they propose in the [Akka Streams documentation][akka-streams-doc]. Following their proposal we could do something like:
{% highlight scala %}
def inParallel[A, B, Mat](parallelism: Int, flow: Graph[FlowShape[A, B], Mat]): Flow[A, B, NotUsed] =
  Flow.fromGraph(GraphDSL.create() { implicit builder =>
    val dispatch = builder.add(Balance[A](parallelism))
    val merge = builder.add(Merge[B](parallelism))
    (0 until parallelism).foreach { i => 
      dispatch.out(i) ~> flow.async ~> merge.in(i)
    }
    FlowShape(dispatch.in, merge.out)
  })
{% endhighlight %}

In order to allow the flow to be executed in parallel we need to use the method `async` that will be explained in the next section.
If we build the new runnable graph we have:
{% highlight scala %}
val downloadFilesInParallel = inParallel(2, downloadFile)

fileUrls
  .via(downloadFilesInParallel)
  .via(countWords)
  .via(updateFile)
  .runWith(Sink.ignore)
{% endhighlight %}

But we can see that the output looks different, closer to what we want to achieve: 

{% include custom/20180805/graph3.html %}

{% highlight markdown %}
Starting [Download file] for [item 1]
Starting [Download file] for [item 17]
Finishing [Download file] for [item 1]
Finishing [Download file] for [item 17]
Starting [Download file] for [item 18]
Starting [Count words] for [item 1]
Starting [Download file] for [item 2]
Finishing [Count words] for [item 1]
Starting [Update file] for [item 1]
Finishing [Update file] for [item 1]
Starting [Count words] for [item 17]
Finishing [Count words] for [item 17]
Starting [Update file] for [item 17]
...
{% endhighlight %}

The second pair of files starts to be downloaded right after the first couple of files finishes, this is a consequence of using `async`.
However the count words and update file steps are still executed sequentially.
The order of how the items are executed is modified by the `Balance` component used in the custom graph.

## Pipelining or vertical parallelism
This refers to executing different steps of the graph in parallel. We can achieve this thanks to the method mentioned before, `async`, that allows us
to mark a step of the graph as asynchronous, which means that the operation will be executed by a different actor and therefore in parallel.

In the previous custom graph we are marking every `downloadFile` flow as async and we have two. This means that we will have two actors
only downloading files, and once they are done they'll want to keep downloading files, so they'll ask the source for more items even when the rest of the
pipeline is not finished, because it is a different actor who is executing the rest of the process.

Then, in order to achieve a similar behaviour with `mapAsync` we can create the graph using the `async` method:
{% highlight scala %}
fileUrls
  .via(downloadFileAsync.async)
  .via(countWords)
  .via(updateFile)
  .runWith(Sink.ignore)
{% endhighlight %}

And we have the expected output:

{% include custom/20180805/graph4.html %}

{% highlight markdown %}
Starting [Download file] for [item 1]
Starting [Download file] for [item 2]
Finishing [Download file] for [item 2]
Finishing [Download file] for [item 1]
Starting [Count words] for [item 1]
Starting [Download file] for [item 3]
Starting [Download file] for [item 4]
Finishing [Count words] for [item 1]
Starting [Update file] for [item 1]
Finishing [Update file] for [item 1]
Starting [Count words] for [item 2]
...
{% endhighlight %}

The main difference between the custom graph and `mapAsync` is that the latter executes the task inside the `Future` and in the completion callback pushes the item to the stream,
while the custom graph lets the actor execute the logic.
If we decide to use the custom graph, we could achieve an acceptable final solution like this (if we don't care about order):
{% highlight scala %}
val downloadFilesInParallel = inParallel(2, downloadFile)

fileUrls
  .via(downloadFilesInParallel)
  .via(countWords.async)
  .via(updateFile)
  .runWith(Sink.ignore)
{% endhighlight %}
Why? Having in mind the requirements at the top of the page:

- We want to update the file sequentially, and it takes ~1 second for each update.
- The time to download one file is of ~2 seconds, so we have time to update the output file twice.
- If we download two files in parallel, we can update the file with those two files while we are downloading the next two files.
- The time that the `countWords` operation needs is very small, it will only introduce a delay at the beginning, that we are removing in the
next iterations making this step async, because, after the output file is updated, it will have the next result ready to be stored.

{% include custom/20180805/graph5.html %}

Then, at some point, we would be processing one file per second instead of one file every 3.1 seconds, that was how we started.

## Horizontal parallelism in substreams

Now imagine we want to write to different files depending on, for example, the language of the input file.
We can do a first implementation:

{% highlight scala %}
val supportedLanguages = 3 // number of possible languages
def getLanguage(item: String) = (item.toInt % supportedLanguages + 1).toString
val updateFile = Flow[String].map { item => 
  task("Update file " + getLanguage(item), 1000)(item)
}

results
  .groupBy(supportedLanguages, getLanguage)
  .via(updateFile)
  .mergeSubstreams
  .runWith(Sink.ignore)
{% endhighlight %}

The output we obtain is the following:

{% include custom/20180805/graph6.html %}

{% highlight markdown %}
Starting [Update file 1] for [item 3]
Finishing [Update file 1] for [item 3]
Starting [Update file 3] for [item 2]
Finishing [Update file 3] for [item 2]
Starting [Update file 2] for [item 1]
Finishing [Update file 2] for [item 1]
Starting [Update file 2] for [item 4]
...
{% endhighlight %}

So in this case we are not getting any improvement with the `groupBy`, we are still executing it sequentially.
It could make some sense if we are also combining it with some other grouping operations but what we want to achieve here is writing
to the different files in parallel.

As we saw in the previous examples, we can use the `async` method, so we can have an actor processing each substream of files:
{% highlight scala %}
results
  .groupBy(supportedLanguages, getLanguage)
  .via(updateFile.async)
  .mergeSubstreams
  .runWith(Sink.ignore)
{% endhighlight %}

And the output will be the expected:

{% include custom/20180805/graph7.html %}

{% highlight markdown %}
Starting [Update file 1] for [item 3]
Starting [Update file 2] for [item 1]
Starting [Update file 3] for [item 2]
Finishing [Update file 1] for [item 3]
Finishing [Update file 2] for [item 1]
Starting [Update file 2] for [item 4]
Finishing [Update file 3] for [item 2]
Starting [Update file 3] for [item 5]
Starting [Update file 1] for [item 6]
Finishing [Update file 2] for [item 4]
...
{% endhighlight %}

### More

For more information about parallelism and pipelining check the [Akka Streams documentation][akka-streams-doc].

[akka-streams-doc]: https://doc.akka.io/docs/akka/current/stream/stream-parallelism.html?language=scala