---
layout: post
title:  "Validate configuration as a case class with Cats and Shapeless"
date:   2018-08-22 18:34:40
image:  "/images/20180822/icon.png"
level: Intermediate
categories: Shapeless
---

Probably you are familiar with the format of a `.conf` file like this one:
{% highlight conf %}
my-config {
  host = "localhost"
  port = 9000
  list-of-ints = [1, 2, 3, 4]
  list-of-strings = ["a", "b", "c"]
  list-of-objects = [
    {
      name = "some name"
      value = true
    },
    {
      name = "some other name"
    }
  ]
  test-map = {
    key = "testValue"
  }
}
{% endhighlight %}

The final purpose of this post will be to map the `Config` instance or any other key-value configuration to these case
classes that represent a configuration, after validating every field and accumulating all the possible errors:
{% highlight scala %}
case class SomeObject(
  name: String,
  value: Option[Boolean]
)

case class MyConfig(
  host: String,
  port: Int,
  listOfInts: List[Int],
  listOfStrings: List[String],
  listOfObjects: List[SomeObject],
  testMap: Map[String, String]
)
{% endhighlight %}

The tools that will be used here are:

- Type classes in order to have a config reader for every type and being able to easily create readers for custom types.
- Cats `Validated` to accumulate the errors.
- Shapeless `LabelledGeneric` to map the different fields to the target case class.

## Type class

The first step is to create the type class we are going to use to read the properties.
Given the methods that `Config` provides, we have two different approaches:

1) Use the methods that require the config, a path and a type to extract the value, for example:
{% highlight scala %}
config.getInt("my-config.port")
config.getStringList("my-config.list-of-string")
...
{% endhighlight %}

2) Work with Java types and validate types with pattern matching:
{% highlight scala %}
val value = config.getValue("my-config.host")
value.valueType match {
  case STRING => value.unwrapped.asInstanceOf[String]
}
...
{% endhighlight %}

In this post I'll go for the former so we don't lose the validation that the TypeSafe Config library already has.
Now we can define a case class to represent the errors, a type that represents the operation result and a trait that
will implement the operation to read the config:
{% highlight scala %}
case class FieldError(key: String, cause: Throwable)

type ValidatedConfig[T] = ValidatedNel[FieldError, T] // From cats library

trait ConfigReader[T] {
  def read(key: String, config: Config): ValidatedNel[FieldError, T]
}
{% endhighlight %}

So that we can achieve something like this:

![Description][diagram1]

## Simple types

We can implement the `ConfigReader` for `Int` like this:
{% highlight scala %}
import Validated._

implicit val intConfigReader: ConfigReader[Int] = (key, config) =>
  Try(config.getInt(key)).fold(
    error => invalidNel(FieldError(key, error)),
    success => valid(success)
  )
{% endhighlight %}
If everything goes fine, it returns `Valid[Int]` otherwise `Invalid[NonEmptyList[FieldError]]`.
We can abstract this code so we avoid to repeat it in the remaining implementations.
{% highlight scala %}
// Common code
private def toHyphenSeparated(key: String): String =
    key.split("(?=[A-Z])").map(_.toLowerCase).mkString("-")

private def validatedConfig[T](key: String, getValue: String => T): ValidatedConfig[T] =
  Try(
    getValue(toHyphenSeparated(key))
  ).fold(
    error => invalidNel(FieldError(key, error)),
    valid
  )

// (Some) Native config readers
implicit val intConfigReader: ConfigReader[Int] =
  (key, config) => validatedConfig(key, config.getInt)
  
implicit val strConfigReader: ConfigReader[String] =
  (key, config) => validatedConfig(key, config.getString)
  
implicit val blnConfigReader: ConfigReader[Boolean] =
  (key, config) => validatedConfig(key, config.getBoolean)
{% endhighlight %}

And now we can do:
{% highlight scala %}
val config = ConfigFactory.load()
implicitly[ConfigReader[String]].read("myConfig.host", config)
{% endhighlight %}

That will give us:
{% highlight txt %}
Valid(localhost)
{% endhighlight %}

But if we do:
{% highlight scala %}
val config = ConfigFactory.load()
implicitly[ConfigReader[Int]].read("myConfig.host", config)
{% endhighlight %}

We'll get an error:
{% highlight txt %}
Invalid(
  NonEmptyList(
    FieldError(
     myConfig.host,
     com.typesafe.config.ConfigException$WrongType: application.conf: 28: my-config.host has type STRING rather than NUMBER
    )
  )
)
{% endhighlight %}

## Option

This is the first reader that will compose with another readers. We can use the `hasPath` method from `Config` to check
if a path exists or not and then return `Some` or `None`:
{% highlight scala %}
implicit def optConfigReader[T](implicit reader: ConfigReader[T]): ConfigReader[Option[T]] =
  (key, config) =>
    if (config.hasPath(toHyphenSeparated(key))) reader.read(key, config).map(Some.apply)
    else valid(None)
{% endhighlight %}

We define a `ConfigReader` for `Option[T]`, where T can be anything. Then if we require the `ConfigReader[Option[Int]]`
it will look for the `ConfigReader[Int]` in the context so it can be used. For example:

{% highlight scala %}
val config = ConfigFactory.load()

implicitly[ConfigReader[Option[String]]].read("myConfig.host", config)
implicitly[ConfigReader[Option[String]]].read("myConfig.invalidHost", config)
{% endhighlight %}
Will return respectively:
{% highlight scala %}
Valid(Some(localhost))
Valid(None)
{% endhighlight %}

## Map

Following the same point of view, in order to build a `Map` we would need to provide a key and a type to extract
every value of the map which we can get easily from the config:
{% highlight scala %}
import scala.collection.JavaConverters._

implicit def mapConfigReader[T](implicit reader: ConfigReader[T]): ConfigReader[Map[String, T]] =
  (key, config) =>
    validatedConfig(key, config.getConfig).toEither.flatMap { nextConfig =>
      nextConfig.entrySet.asScala.toList.map(
        entry => reader.read(entry.getKey, nextConfig).map(entry.getKey -> _)
      ).sequence[ValidatedConfig, (String, T)].map(_.toMap).toEither
    }.toValidated
{% endhighlight %}
In this example the key is always supposed to be a `String`.

The first step is validating the call to `config.getConfig`. If the specified path is not an object it will fail.
It is converted to `Either` because in this case we don't want to make the validation in parallel: if this object
is invalid we can't continue processing it.

Then we get all the entries of the object with `entrySet` and we convert it to a Scala `List`. We can use every entry
to get the configuration using the respective reader that is included implicitly. From this we obtain a `List[ValidatedConfig[_]]`
so we can use the implicit method `sequence` from cats to make it a `ValidatedConfig[List[_]]`.

## List
For lists, we don't have a method that gives us a `List[Config]` that we can map to `List[T]` providing some key. What we
have are the `Config` methods `getIntList`, `getStringList`, etc. so we could do a first implementation using them:
{% highlight scala %}
implicit val intLstConfigReader: ConfigReader[List[Int]] =
  (key, config) => validatedConfig(key, config.getIntList).map(_.asScala.toList.map(_.toInt))
  
implicit val strLstConfigReader: ConfigReader[List[String]] =
  (key, config) => validatedConfig(key, config.getStringList).map(_.asScala.toList)
 
 ...
{% endhighlight %}

Or we could create a composable implementation doing a little trick, where we transform every `ConfigValue` of the list
to a `Config` where we set some key (in this case the index) and we pass it to the `read` method.

{% highlight scala %}
implicit def lstConfigReader[T](implicit reader: ConfigReader[T]): ConfigReader[List[T]] =
    (key, config) =>
      validatedConfig(key, config.getList).toEither.flatMap(
        _.iterator().asScala.zipWithIndex.map { case (value, index) =>
          val nextConfig =
            ConfigFactory.empty().withValue(index.toString, value)
          reader.read(index.toString, nextConfig)
        }.toList.sequence[ValidatedConfig,  T].toEither
      ).toValidated
{% endhighlight %}

This implementation is very similar to the `ConfigReader[Map[String, T]]` implementation but we discard the key that we've
just created in this case.

## HList and Case Class

To implement the reader for any case class we define a reader that depends on the reader for the output of the
respective `LabelledGeneric`:

{% highlight scala %}
implicit def caseClassReader[T, L <: HList](
  implicit gen: LabelledGeneric.Aux[T, L],
  configReader: ConfigReader[L]
): ConfigReader[T] =
  (key, config) => configReader.read(key, config).map(gen.from)
{% endhighlight %}

This L is a `HList` that will be something like:

`FieldKey[K1, T1] :: ... :: FieldKey[Kn, Tn] :: HNil`

So we can see we need a reader that depends recursively on itself for `FieldKey[K, H] :: T` where `T <: HList` and
for `HNil`.

The `ConfigReader` for `HNil` will never return an error:
{% highlight scala %}
implicit val hnilReader: ConfigReader[HNil] = 
  (key, config) => Valid(HNil)
{% endhighlight %}
  
For the `FieldType[K, H] :: T` reader we need to use the reader for the head of the `HList` that will be one of the
previously created and the reader for the Tail, that will be the `HNil` reader or the same reader but for the tail of the
`HList`.
{% highlight scala %}
import shapeless.labelled.field

implicit def hlistReader[K <: Symbol, H, T <: HList](
  implicit headReader: ConfigReader[H],
  tailReader: Lazy[ConfigReader[T]],
  witness: Witness.Aux[K]
): ConfigReader[FieldType[K, H] :: T] = (key, config) =>
    validatedConfig(key, config.getConfig).toEither.flatMap { nextConfig =>
      val head: ValidatedConfig[H] = headReader.read(witness.value.name, nextConfig)
      val tail: ValidatedConfig[T] = tailReader.value.read(key, config)

      (head, tail).mapN((head, tail) => field[K](head) :: tail).toEither
    }.toValidated
{% endhighlight %}
The output of the readers is combined with `mapN`.
Using `field[K]` we can associate the read type with its key, so that this key can be used to build the case class
using the `LabelledGeneric`.
For the tail reader `Lazy[T]` is needed to avoid divergence when it's recursive.

## Final details

Now that we have all the implicit main `ConfigReader` instances, we can create an implicit class to add a custom
method `read[T](path: String)` to `Config`:
{% highlight scala %}
implicit class ReadableConfig(config: Config) {
  def read[T](path: String)(implicit reader: ConfigReader[T]): Either[NonEmptyList[FieldError], T] =
    reader.read(path, config).toEither
}
{% endhighlight %}

And we can create a validated instance of our case class doing:
{% highlight scala %}
val config = ConfigFactory.load()

val myConfig =  ConfigFactory.load().read[MyConfig]("my-config")
{% endhighlight %}

And we would get:
{% highlight scala %}
Right(
  MyConfig(
    localhost,
    9000,
    List(1, 2, 3, 4),
    List(a, b, c),
    List(
      SomeObject(some name,Some(true)),
      SomeObject(some other name,None)),
    Map(key -> testValue)
  )
)
{% endhighlight %}

But if we have this config in our `.conf` file:
{% highlight conf %}
bad-config {
  host = "localhost"
  port = "port"
  list-of-strings = ["a", 2, "c"]
  list-of-objects = [
    {
      name = "some name"
      value = 2
    },
    {
      value = false
    }
  ]
}
{% endhighlight %}

And try to load it the same way:
{% highlight scala %}
val myConfig = config.read[MyConfig]("bad-config")
{% endhighlight %}

We can see all the errors are being collected so we can try to correct them in just one go.
{% highlight scala %}
Left(
  NonEmptyList(
    FieldError(port,com.typesafe.config.ConfigException$WrongType: application.conf: 48: port has type STRING rather than NUMBER),
    FieldError(listOfInts,com.typesafe.config.ConfigException$Missing: No configuration setting found for key 'list-of-ints'),
    FieldError(value,com.typesafe.config.ConfigException$WrongType: application.conf: 53: value has type NUMBER rather than BOOLEAN),
    FieldError(name,com.typesafe.config.ConfigException$Missing: No configuration setting found for key 'name'),
    FieldError(testMap,com.typesafe.config.ConfigException$Missing: No configuration setting found for key 'test-map')
  )
)
{% endhighlight %}

### More

You can check the full code [here][full-code].

For more information about Cats and Validated data type check the [Cats documentation][cats-validated-doc].

Also for more information about Shapeless and LabelledGeneric check the [Underscore Shapeless Guide][shapeless-book].

[cats-validated-doc]: https://typelevel.org/cats/datatypes/validated.html
[shapeless-book]: https://underscore.io/books/shapeless-guide/
[full-code]: https://gist.github.com/algd/f79e2d9517f15f538b523f0ef66ba21f
[diagram1]: /images/20180822/diagram1.png