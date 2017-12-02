module HeroesCowards.Renderer (
    renderHeroesCowardsFrame
  ) where

import HeroesCowards.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type HeroesCowardsRenderFrame = RenderFrame HACAgentState HACEnvironment
type HeroesCowardsAgentColorer = AgentColorerCont2d HACAgentState

renderHeroesCowardsFrame :: HeroesCowardsRenderFrame
renderHeroesCowardsFrame = renderFrameCont2d 
                                (defaultAgentRendererCont2d 2 heroesCowardsAgentColor hacCoord)
                                voidEnvRendererCont2d

heroesCowardsAgentColor :: HeroesCowardsAgentColorer
heroesCowardsAgentColor HACAgentState { hacRole = role } = heroesCowardsAgentColorAux role
    where   
        heroesCowardsAgentColorAux :: HACRole -> GLO.Color
        heroesCowardsAgentColorAux Hero = GLO.green
        heroesCowardsAgentColorAux Coward = GLO.red