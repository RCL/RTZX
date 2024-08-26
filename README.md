# RTZX

-------------------------------------------------------------------------------
 RTZX 1KiB procedural gfx - a contribution to Chaos Constructions 2024
 		by RCL of Virtual Vision Group.

 Works on ZX Spectrum 48K and above.

 Source code in Z80 assembly.

-------------------------------------------------------------------------------

## Algo

  A 85 x 64 grid is traced, and in each grid point we get a [0..16] brightness 
value from the raytracer. To stretch the result to the whole 256 x 192 screen,
we proceed to render as many dots around each grid point as its brightness, using
a very crude Gaussian approximation to disperse them, and also set the attributes
accordingly. So a grid point with a value of 0 will remain black, while a grid
point with a value of 9 will have 9 points rendered around it, with them being
concentrated primarily in its center.

## Background

  This is an honest raytracer, at least for a certain definition of honesty - 
the only inputs are the camera position and 4 geometric primitives (3 spheres 
and a plane). Some assumptions and tweaks have been made to make the calculation
numerically easier on the poor Z80.

  My inspiration was this year's (2024) article by Gabriel Gambetta
(https://gabrielgambetta.com/zx-raytracer.html), which I used as a starting 
point. However its floating point calculations would not allow me to get anywhere
close to 1 minute calculation time limit imposed by the rules of the compo, so
calculations are done in a signed 9.7 fixed point. You can see the jagged edges
and other artifacts due to that.

Also, I found out that none of the signed 16 bit multiplication and division 
routines (Z80 does not have mul/div ops in case you weren't aware) that
I could find on the web worked, so I had to go back to the basics and implement
them myself (perhaps not in an optimal way). If you want to improve upon then,
please go ahead.

I have also written a C program on PC to debug the algorithm and tweak it
numerically. I am not including that (yet?) as it is a part of my larger
experimentation framework - maybe one day. You may need to calculate the values
in Spheres table by hand if you want to change the positions of the spheres (they
all are essentially floats multiplied by 128 and converted to shorts).

To draw the screen,the code does about 11k divisions and 148.5k multiplications,
of which a lot are multiplications with 0 (13k) or 8x8 bit (85k), so fast paths
exist for those. Most loops are unrolled both for speed and better compression.

Speaking of which, you may be wondering how the hell this fit 1024 bytes. Well,
just like most other size-limited entries, the code is compressed, and a small
kickstart of 70 something bytes is prepended to decompress it on start. When
decompressed, the main code is 3604 bytes. To achieve good compression I had to
use the same registers the same way as much as possible and duplicate blocks of
code even when more elegant solutions could exist, so take that into account when 
judging the source.

## Credits

 This code is by RCL/VVG and hereby I am placing it into the public domain.

## Extra credits

 - Einar Saukas for the ZX0 compressor (https://github.com/einar-saukas/ZX0).
 - Viper of TechnoLab for the 1998 article describing a 73 cycle plot routine (https://zxpress.ru/article.php?id=8242).
   I have seen an even faster 69 cycle one that he mentions, but couldn't find it nor had
   the time to reinvent it.
 - Unnamed authors of a combined LFSR/LCG random generator (https://wikiti.brandonw.net/index.php?title=Z80_Routines:Math:Random#Combined_LFSR.2FLCG.2C_32-bit_seeds)
 - Multiple bloggers who put out various small Speccy routines that come handy
   (like https://espamatica.com/zx-spectrum-screen/#attribute-address-from-character-coordinates). Don't stop, there's never enough.
 - Of course, as already mentioned, Gabriel Gambetta for both the inspiration
    and providing a starting point with his BASIC code (https://gabrielgambetta.com/zx-raytracer.html)
