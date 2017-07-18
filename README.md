# GifId

This is a simple project I used to:
1. Teach myself Scala
2. Learn a bit about gifs
3. Learn a little about image fingerprinting

As a result of #1, it's not great code, so what are you doing here?

The goals of #3 are to create an image fingerprinting system that is robust in its ability to detect images that
have been altered via resizing, slightly altered via noise, color processing, adding/removing frames, or very
slightly altered via cropping. If inspired, I might take this code and make it cleaner and more robust, since
it actually seems to accomplish these goals.

Performance-wise, this code is horrible. Image processing really requires SMD, which is not available in the JVM.
