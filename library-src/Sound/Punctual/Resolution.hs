module Sound.Punctual.Resolution where

data Resolution =
  QHD |
  FHD |
  HD |
  WSVGA |
  SVGA |
  VGA |
  QVGA
  deriving (Eq,Ord,Show)

pixels :: Num a => Resolution -> (a,a)
pixels QHD = (2560,1440)
pixels FHD = (1920,1080)
pixels HD = (1280,720)
pixels WSVGA = (1024,576)
pixels SVGA = (800,600)
pixels VGA = (640,480)
pixels QVGA = (320,240)
