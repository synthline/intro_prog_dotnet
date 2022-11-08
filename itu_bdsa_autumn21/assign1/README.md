# Assignment 1

## C&#35;

Clone this repository and bring the code pieces you need into your BDSA Assignments GitHub repository.

### Generics

Compare the following two methods:

```csharp
int GreaterCount<T, U>(IEnumerable<T> items, T x)
where T : IComparable<T>

int GreaterCount<T, U>(IEnumerable<T> items, T x)
where T : U
where U : IComparable<U>
```

Both methods returns the amount of elements in `items` which are *greater than* `x`.

Explain in your own words what the type constraints mean for both methods.

#### Optional

Implement and test the latter of the two methods including a type hierarchy which supports the given type constraints.

### Iterators

Implement and test the following methods:

```csharp
IEnumerable<T> Flatten<T>(IEnumerable<IEnumerable<T>> items)

IEnumerable<T> Filter<T>(IEnumerable<T> items, Predicate<T> predicate)
```

1. `Flatten` takes as argument a stream of a stream of `T`'s. It should return a stream of `T`'s.

1. `Filter` takes as arguments a stream of `T`'s and a function which returns `true` or `false` when applied to an instance of `T`. It returns a stream of only the `T`s where the predicate returns `true`.

#### Notes

You must `yield` elements and not use a temporary in-memory collection.

You can declare a `Predicate` likes this:

```csharp
public static void Main(string[] args)
{
    Predicate<int> even = Even;
}

public static bool Even(int i)
{
    return i % 2 == 0;
}
```

### Regular Expressions

Implement and test the following methods:

```csharp
IEnumerable<string> SplitLine(IEnumerable<string> lines)

IEnumerable<(int width, int height)> Resolutions(IEnumerable<string> resolutions)

IEnumerable<string> InnerText(string html, string tag)
```

1. `SplitLine` takes as argument a stream of lines (strings) and returns a stream of the words on those lines (also strings).
A 'word' is a non-empty contiguous sequence of the letters a–z or A–Z or the digits 0–9. Use a regular expression to split the lines into words.

1. `Resolutions` takes a string containing resolutions. A resolution could be `1920x1080`. It returns a stream of resolutions as tuples, e.g. `(1920, 1080)`. The solution must use *named capture groups*.

   Sample input (one line represents an element in the `IEnumerable`):

    ```txt
    1920x1080
    1024x768, 800x600, 640x480
    320x200, 320x240, 800x600
    1280x960
    ```

    Sample output:

    ```txt
    (1920, 1080)
    (1024, 768)
    (800, 600)
    (640, 480)
    (320, 200)
    (320, 240)
    (800, 600)
    (1280, 960)
    ```

1. `InnerText` takes as arguments a string containing HTML and a specific tag name. It returns the *inner text* of each of those tags. Use a regular expression with a *back reference* to match tags.

#### Notes

You must `yield` elements and not use a temporary in-memory collection.

Given the following `html` and the tag `a`:

```html
<div>
    <p>A <b>regular expression</b>, <b>regex</b> or <b>regexp</b> (sometimes called a <b>rational expression</b>) is, in <a href="/wiki/Theoretical_computer_science" title="Theoretical computer science">theoretical computer science</a> and <a href="/wiki/Formal_language" title="Formal language">formal language</a> theory, a sequence of <a href="/wiki/Character_(computing)" title="Character (computing)">characters</a> that define a <i>search <a href="/wiki/Pattern_matching" title="Pattern matching">pattern</a></i>. Usually this pattern is then used by <a href="/wiki/String_searching_algorithm" title="String searching algorithm">string searching algorithms</a> for "find" or "find and replace" operations on <a href="/wiki/String_(computer_science)" title="String (computer science)">strings</a>.</p>
</div>
```

The `InnerText` method should return:

- theoretical computer science
- formal language
- characters
- pattern
- string searching algorithms
- strings

You should support nested html tags such that given the following `html` and the tag `p`:

```html
<div>
    <p>The phrase <i>regular expressions</i> (and consequently, regexes) is often used to mean the specific, standard textual syntax for representing <u>patterns</u> that matching <em>text</em> need to conform to.</p>
</div>
```

The `InnerText` method should return:

>The phrase regular expressions (and consequently, regexes) is often used to mean the specific, standard textual syntax for representing patterns that matching text need to conform to.

## Software Engineering

### Exercise 1

What is meant by “knowledge acquisition is not sequential”? Provide a concrete example of knowledge acquisition that illustrates this.

### Exercise 2

Specify which of the following decisions were made during requirements or system design:

- “The ticket distributor is composed of a user interface subsystem, a subsystem for computing tariff, and a
network subsystem managing communication with the central computer.”
- “The ticket distributor will use PowerPC processor chips.”
- “The ticket distributor provides the traveler with an on-line help.”

### Exercise 3

In the following description, explain when the term account is used as an application domain concept and when as a solution domain concept:
> "Assume you are developing an online system for managing bank accounts for mobile customers. A major design issue is how to provide access to the accounts when the customer cannot establish an online connection. One proposal is that accounts are made available on the mobile computer, even if the server is not up. In this case, the accounts show the amounts from the last connected session."

### Exercise 4

A passenger aircraft is composed of several millions of individual parts and requires thousands of persons to assemble. A four-lane highway bridge is another example of complexity. The first version of Word for Windows, a word processor released by Microsoft in November 1989, required 55 person-years, resulted into 249,000 lines of source code, and was delivered 4 years late. Aircraft and highway bridges are usually delivered on time and below budget, whereas software is often not. Discuss what are, in your opinion, the differences between developing an aircraft, a bridge, and a word processor, which would cause this situation.

### Exercise 5

Specify which of these statements are functional requirements and which are nonfunctional requirements:

- “The TicketDistributor must enable a traveler to buy weekly passes.”
- “The TicketDistributor must be written in Java.”
- “The TicketDistributor must be easy to use.”
- “The TicketDistributor must always be available.”
- “The TicketDistributor must provide a phone number to call when it fails.”

### Exercise 6

What is the purpose of modeling?

## Submitting the assignment

To submit the assignment you need to create a .pdf document using LaTeX containing the answers to the questions and a link to a public repository containing your fork of the completed code.
