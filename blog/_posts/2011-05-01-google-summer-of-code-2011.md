---
layout: post
title: "Google Summer of Code 2011"
tags: [computer graphics]
vanity: "2011-05-01-google-summer-of-code-2011"
---
{% include blog_vars.html %}

<figure class="image_float_left">
  <img src="{{resources_path}}/brl-cad_logo.png" alt="BRL-CAD Logo" />
</figure>

This Monday the result came out that my project was selected in [Google Summer of Code 2011](https://www.google-melange.com/archive/gsoc/2011)! I submitted two proposals to the [BRL-CAD](https://brlcad.org/), which is an organization that develops a solid modeling program through [constructive solid geometry](https://en.wikipedia.org/wiki/Constructive_solid_geometry).

In addition to the modeler, the software has other tools the main one being a ray tracer to generate images (2D) from solids (3D).

My first proposal was to port existing standalone image processing applications to a library. This is essentially a refactoring. There are not many difficulties, but it is a laborious project.

The second proposal, the one that was accepted, was to improve the BRL-CAD shader system. Simplistically, we can say that a shader defines the texture of a 3D object. We can implement shaders to represent several types of materials like glass, cloud, fire, etc.

## Ray Tracing

A shader is directly related to ray tracing. A ray tracer generates 2D images from 3D images, simulating the behavior of light. Imagine a light source like a light bulb for example. It emits infinite rays of light in various directions. Some of these rays will hit objects, some will come directly to our eyes, and some will go in other directions.

Rays striking objects will be reflected/refracted to a greater or lesser extent (for example, a ray incident to a black cloth will hardly be reflected; on a metallic surface almost all the incoming ray will be reflected; on a glass jar the ray will be part refracted and part reflected).

<figure class="center_children">
  <img src="{{resources_path}}/ray-tracing.jpeg" alt="Scene created via ray-tracing" />
  <figcaption>Scene created via ray-tracing.</figcaption>
</figure>

The image that we create of the object is the result of the rays that hit this object and go to our eye. This is a very simplified description of the behavior of light. Imagine that we want to simulate this behavior computationally. For each light source we would have to emit many rays of light and simulate the path it would take.

However, it is certain that most of these rays would never reach our eyes (or would arrive with negligible intensity), so computing every single ray path emanating from light sources would be a huge computational waste.

The trick is to do the opposite! Make the light go the other way, starting from the eyes! We simulate the path taken by a ray and when it arrives at a source of light, we will know what the color of that ray is. In this way, we only simulate rays that actually reach the eye.

## Shaders

Where do shaders fit into this story? We have seen that shaders define material properties. In general, each surface is associated with a shader. When a ray hits this surface, the shader will define how much of the ray will be reflected, how much will be refracted, what color the surface is, etc. according to the material/texture it represents.

<figure class="center_children">
  <img src="{{resources_path}}/shaders.jpeg" alt="Textures corresponding to different shaders." />
  <figcaption>Textures corresponding to different shaders.</figcaption>
</figure>

## Open Shading Language

Currently BRL-CAD shaders are implemented in C and compiled as dynamic libraries. The ray tracer dynamically loads the libraries that will be needed to render the scene. The main idea of ​​my proposal is to implement a new shader system that allows the use of shaders written in [OSL](http://opensource.imageworks.com/osl.html).

<figure class="image_float_left">
  <img src="{{resources_path}}/osl.jpeg" alt="BRL-CAD Logo" />
</figure>

OSL is basically a language for writing shaders. The code was initially developed by Sony, who made it public in 2010. The language's differential is that it is developed especially for ray tracers, so it employs techniques that promise to improve the performance of these algorithms. The package comes with a compiler for shaders written in OSL (`.osl` format) and converted to `.oso` format. It also comes with a library for manipulating shaders.

## Conclusion

The project itself is quite challenging as until a month ago I had never heard of shaders. Besides, I have to reconcile this project with the end of my master's, so until August 15th I'll have to put in a lot of effort.

I am very excited about this project especially as it involves the study of ray tracing, which I have a great interest in. Ideally I would like to finish this project and get involved with other BRL-CAD projects (or other organizations like Yafaray) involving ray tracers.

I will focus the blog posts on this project, taking the opportunity to write about its progress. I hope it will be all right!

## Notes

This is a review and translation done in 2022 of my original post in Portuguese: [Google Summer of Code 2011](https://kuniga.wordpress.com/2011/05/01/google-summer-of-code-2011/).
