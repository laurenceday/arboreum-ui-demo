# Arboreum Demo

Hi! If you're here from the general public - you'll want to look here: http://3.10.144.126:3838/demo/arboreum/

Our site is http://www.arboreum.finance, but the concrete details you're looking for are available in our... light-yellow? off-white? paper, available here: https://www.dropbox.com/s/duqearsi7w1i0w1/A_Proposal_For_Risk_Distribution_Over_Intelligent_Credit_Networks.pdf?dl=0

This is actually the least exciting bit of Arboreum that exists right now - it's a proof of concept written in R, with (at present) zero degrees of freedom. Nonetheless...

- Front-end code is in /app.
- Back-end code is in /app/src.
- Data structures displayed throughout are in /app/pregenerated.

Current setup is tweaked for AWS hosting rather than launching R project directly.

The data structures used here are generated from /app/src/DemoWorkflow.R - if you want, you can generate your own data structures (i.e. a change in parameters et al), although this is something due for later versions of our demo anyway!
