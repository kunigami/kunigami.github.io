---
layout: doc
title: "Maven Cheatsheet"
---

## Setup

## Create a project structure

{% highlight bash %}
mvn archetype:generate -DgroupId=com.example -DartifactId=my-java-project -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false
{% endhighlight %}

## Template file

`App.java`:

{% highlight java %}
public class App
{
    public static void main( String[] args )
    {
        System.out.println("Hello World!");
    }
}
{% endhighlight %}

## Configure build

`pom.xml`:

{% highlight xml %}
<project>
    <build>
        <plugins>
            <plugin>
                <groupId>org.codehaus.mojo</groupId>
                <artifactId>exec-maven-plugin</artifactId>
                <version>3.0.0</version>
                <executions>
                    <execution>
                        <goals>
                            <goal>java</goal>
                        </goals>
                    </execution>
                </executions>
                <configuration>
                    <mainClass>com.example.App</mainClass>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
{% endhighlight %}

## Configure dependencies

`pom.xml`:

{% highlight xml %}
<project>
    <dependencies>
        <dependency>
            <groupId>junit</groupId>
            <artifactId>junit</artifactId>
            <version>3.8.1</version>
            <scope>test</scope>
        </dependency>
    </dependencies>
</project>
{% endhighlight %}

## Operational

### Compile

{% highlight bash %}
mvn compile
{% endhighlight %}

### Run

`java` is the name provided in `<goal>` on the build config.

{% highlight bash %}
mvn exec:java
{% endhighlight %}
