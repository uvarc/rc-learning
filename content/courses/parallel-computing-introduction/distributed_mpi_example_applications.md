---
date: "2020-11-17"
title: "Application Examples"
weight: 170
---

One of the most common categories of application that frequently uses parallel computing is the numerical solution of partial differential equations. A partial differential equation, or _PDE_, is an equation containing derivatives of a function of two or more variables.  This is in contrast to ordinary differential equations, which are equations containing derivatives of a function of one variable.

Some examples of phenomena that are modeled by PDEs include:
* Air flow over an aircraft wing
* Blood circulation in the human body
* Water circulation in an ocean
* Bridge deformations as its carries traffic
* Evolution of a thunderstorm
* Oscillations of a skyscraper hit by an earthquake
* Strength of a toy

Partial differential equations are solved numerically by several approaches.  Two particularly common ones are the _finite element method_ (FEM) and the _finite difference method_. A simple conceptual explanation of the difference between these two methods is that finite elements approximates the solution and attempts to fit it to the conditions, whereas the finite-difference method approximates the equations and solves a discrete version of the original system.

We will focus on examples that use finite differencing, since this method is widely used in some areas and is a typical application for point-to-point messaging because it generally involves solving the equations on some form of grid, as we have been studying.

## Linear Second-Order PDEs

Many physical phenomena can be modeled by linear second-order PDEs. Consider such an equation in the two variables $x$ and $y$.  The general form is

$$A \dfrac{\partial^{2}{u}}{\partial{x}^{2}} + 2B \dfrac{\partial^{2}{u}}{\partial{x}\partial{y}}+C\dfrac{\partial^{2}{u}}{\partial{y}^{2}} + D \dfrac{\partial{u}}{\partial{x}}+E\dfrac{\partial{u}}{\partial{y}}+Fu=G$$

where A, B, C, D E, F, and G are functions of $x$ and $y$ only. The behavior, and appropriate numerical methods, depends only on $B^2-AC$. There are three possibilities:

### Elliptic Equations

In this case, $B^2-AC \lt 0$. This type of equation is often solved by some iterative method.  Elliptic equations occur widely in physical models.

### Parabolic Equations

Here $B^2-AC = 0$. The most widely known example of a parabolic equation is the _diffusion equation_ that describes the diffusion of heat or fluids across a gradient.

### Hyperbolic Equations

For this category, $B^2-AC \gt 0$.  Hyperbolic equations generally represent wave phenomena. 
