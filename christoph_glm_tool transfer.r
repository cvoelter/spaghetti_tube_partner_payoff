

mm1.b.dok2 <- glm(tool_transfer~door + Room.Baited + z.Session + z.Trial
                  , data = dok2.data
                  , family = binomial
) 



mm1.b.dok2.ci=ci.glm(mm1.b.dok2 , resol=100, level=0.95, use="door")