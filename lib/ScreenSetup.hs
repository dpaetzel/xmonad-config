module ScreenSetup where


import Util.XRandR
import Util.XRandR.Types


screenSetup :: String -> IO ()
screenSetup "aristoteles" =
    condChange
        [
            (onlyConnected ["HDMI-0", "DVI-0"],
                [ Output
                    { _name  = "HDMI-0"
                    , _state = Configured
                        { _mode     = Mode (1680, 1050)
                        , _position = (0, 0)
                        , _rotation = RNormal
                        }
                    }
                , Output
                    { _name = "DVI-0"
                    , _state = Configured
                        { _mode     = Mode (1280, 1024)
                        , _position = (1680, -230)
                        , _rotation = RLeft
                        }
                    }
                ]
            )

        ,   (onlyConnected ["DVI-0"],
                [ Output
                    { _name = "DVI-0"
                    , _state = Configured
                        { _mode     = Mode (1280, 1024)
                        , _position = (0, 0)
                        , _rotation = RLeft
                        }
                    }
                ]
            )
        ]


screenSetup "heraklit" =
    condChange
        [
            (onlyConnected ["LVDS1", "VGA1"],
                [ Output
                    { _name  = "LVDS1"
                    , _state = Configured
                        { _mode     = Mode (1366, 768)
                        , _position = (0, 312)
                        , _rotation = RNormal
                        }
                    }
                , Output
                    { _name = "VGA1"
                    , _state = Configured
                        { _mode     = Auto
                        , _position = (1367, 0)
                        , _rotation = RNormal
                        }
                    }
                ]
            )
        ,   (onlyConnected ["LVDS1"],
                [ Output
                    { _name  = "LVDS1"
                    , _state = Configured
                        { _mode     = Mode (1366, 768)
                        , _position = (0, 0)
                        , _rotation = RNormal
                        }
                    }
                , Output
                    { _name = "VGA1"
                    , _state = Disconnected
                    }
                    -- TODO: do this Disconnected-thing when setting only a few
                    -- outputs and leaving others blank
                ]
            )
        ]
