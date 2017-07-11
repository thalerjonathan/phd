module SugarScape.SugarScapeAgent (
    sugarScapeAgentConversation,
    sugarScapeAgentBehaviour
  ) where

import SugarScape.SugarScapeModel
import SugarScape.SugarScapeAgentMonadic
import SugarScape.SugarScapeAgentPure

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentConversation :: SugarScapeAgentConversation 
sugarScapeAgentConversation = sugarScapeAgentConversationM -- sugarScapeAgentConversationPure -- sugarScapeAgentConversationM

sugarScapeAgentBehaviour :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviour = sugarScapeAgentBehaviourM -- sugarScapeAgentBehaviourPure -- sugarScapeAgentBehaviourM
------------------------------------------------------------------------------------------------------------------------