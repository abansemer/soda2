FUNCTION binary_dof, bitimage
   ;FUNCTION to decide whether a binary image is within the depth of field based on the compactness of the image.
   ;This is meant as a rough emulation of DoF rejection used with greyscale probes (1D2D, CIP-G, etc.)

   IF max(bitimage) gt 1 THEN stop, 'Bitimage must be binary'

   ;Convolution kernel, 3x3 for now since mainly concerned with <100um particles.  Center element set to zero.
   k = [[0.7,1,0.7],[1,0,1],[0.7,1,0.7]]     ;Direct neighbors = 1, diagonal neighbors = 1/sqrt(2)
   neighbor_count = total(convol(float(bitimage), k, edge_constant=0) * bitimage)  ;Sum of neighbors

   ;Ratio of neighbor count to area decides whether or not to accept.  Use custom ratios for low pixel counts where
   ;only a few possible arrangements exist, and a fixed ratio for images larger than 6 pixels.
   area = total(bitimage)
   dof = 0
   CASE area OF
      1: dof = 1
      2: IF neighbor_count ge 2 THEN dof = 1
      3: IF neighbor_count ge 5 THEN dof = 1
      4: IF neighbor_count ge 8 THEN dof = 1
      5: IF neighbor_count ge 11 THEN dof = 1
      ELSE: IF neighbor_count ge (area*3) THEN dof = 1
   ENDCASE
   return, dof
END
