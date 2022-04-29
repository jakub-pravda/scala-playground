# Parallel programming

## JVM concurrency and the Java Memory Model
 The ``synchronized`` statement ensure the visibility of writes performed by different threads and to limit concurrent 
access. Generally it is **lock**. Locks ensure that no two threads will execute the same code simultaneously, they
implement **mutual exclusion**. Each object on the JVM has built-in special **monitor lock**.

A **deadlock** occurs when a set of two or more threads acquire resources and then cyclically try to acquire other 
thread's resources without releasing their own 

Establish a total order between resources when acquiring them; this ensures that no set of threads cyclically wait on 
 the resources they previously acquired 
