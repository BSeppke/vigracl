vigracl
=======

Use the power of the computer vision library VIGRA by means of "good old" common lisp. The interaction between both (c++ and functional) worlds is realized by using the CFFI extension and the vigra_c wrapper library. Tested with SBCL! Might work with AllegroCL as well, if you replace the waaf-cffi bindings by the native foreign array passing of ACL.

1. Prerequisites (VIGRA)
-----------------------------------

For Linux and Mac OS X, the vigra Computer Vision library needs to be installed. I recommend the use of a version > 1.9.0, but lower versions may also work. The easiest way to do so, is using your favorite package manager under linux or using MacPorts und  Mac OS X. Otherwise you need to pay attention to install all the needed dependencies on your own.

<b>Attention:</b> Under linux (Ubuntu) I encountered an installation problem of the vigra, such that `vigra-config --libs` pointed to a non-existing file. I was able to solve this by copying the necessary binary to the right position:

    sudo cp /usr/local/lib/libvigraimpex.* /usr/lib/x86_64-linux-gnu

Note, that for Windows, you also need to have installed the MS VC-Runtime (2010) in order to get these binaries running.


2. Prerequisites (Common Lisp)
-----------------------------------

The formerly private development of a vigra-wrapper to (commercial) AllegroCommonLisp only has evolved to work with all major and freely available LISPs, like e.g. CLISP and SBCL. Therefore, you must have already installed the vigra. 
Additionally it relies on the following dependencies, which need to be installed over asdf before the VigraCL may be used:

 * asdf (includes also needed uiop)
 * alexandria (needed by cffi)
 * babel (needed by cffi)
 * trivial-features (needed by cffi)
 * cffi (needed by waaf-cffi)
 * waaf-cffi

3. Installation
-----------------------------------

After setting up asdf and installing, you may add the folder of your VigraCL-Repository to the others w.r.t. the asdf-system. E.g. my ~/.sbclrc (which is loaded, when SBCL is started) looks like:

    (require 'asdf)              
    (push #P"~/development/lisp-packages/babel/" asdf:\*central-registry*)
    (push #P"~/development/lisp-packages/alexandria/" asdf:\*central-registry*)
    (push #P"~/development/lisp-packages/trivial-features/" asdf:\*central-registry*)
    (push #P"~/development/lisp-packages/cffi/" asdf:\*central-registry*)
    (push #P"~/development/lisp-packages/waaf-cffi/" asdf:\*central-registry*)
    (push #P"~/development/vigracl/" asdf:\*central-registry*)

4. Auto-build of the c-wrapper
-----------------------------------

After having set the search paths, you should be able to run the examples provided by the package using e.g.

    sbcl --load examples.lisp 

which should build the vigra_c-wrapper library under linux and mac on the first call and should copy the correct binaries for Windows.

5. Further reading
-----------------------------------

Fur further reading, I recommend one tutorial I gave a few years ago:
<a href="http://kogs-www.informatik.uni-hamburg.de/~seppke/content/research/publications/2010_seppkeetal_els.pdf">Seppke, Benjamin ; Dreschler-Fischer, Leonie: Tutorial: Computer Vision with Allegro Common Lisp and the VIGRA Library using VIGRACL. In: Proceedings of the 3rd European Lisp Symposium, 2010</a>

