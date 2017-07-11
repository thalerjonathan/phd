module SugarScape.Agent (
    sugarScapeAgentConversation,
    sugarScapeAgentBehaviour
  ) where

import SugarScape.Model
import SugarScape.AgentMonadic
import SugarScape.AgentPure

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentConversation :: SugarScapeAgentConversation 
sugarScapeAgentConversation = sugarScapeAgentConversationM -- sugarScapeAgentConversationPure -- sugarScapeAgentConversationM

sugarScapeAgentBehaviour :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviour = sugarScapeAgentBehaviourM -- sugarScapeAgentBehaviourPure -- sugarScapeAgentBehaviourM
------------------------------------------------------------------------------------------------------------------------