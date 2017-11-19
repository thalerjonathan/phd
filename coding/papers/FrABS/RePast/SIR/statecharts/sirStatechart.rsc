<?xml version="1.0" encoding="UTF-8"?>
<xmi:XMI xmi:version="2.0" xmlns:xmi="http://www.omg.org/XMI" xmlns="http://repast.sf.net/statecharts" xmlns:notation="http://www.eclipse.org/gmf/runtime/1.0.2/notation">
  <StateMachine xmi:id="_iFFEQM0EEeepstnKHbY02Q" agentType="sir.SIRStateChartAgent" package="chart" className="SIRStateChart" nextID="181" id="SIR Statechart" uuid="_iFEdMM0EEeepstnKHbY02Q">
    <states xmi:type="PseudoState" xmi:id="_jhv2kM0EEeepstnKHbY02Q" id="Entry State Pointer" type="entry"/>
    <states xmi:type="State" xmi:id="_kBAlcM0EEeepstnKHbY02Q" id="Susceptible" onEnter="agent.makeContact();" uuid="_kBGsEM0EEeepstnKHbY02Q"/>
    <states xmi:type="State" xmi:id="_lBmfwM0EEeepstnKHbY02Q" id="Infected" uuid="_lBmfwc0EEeepstnKHbY02Q"/>
    <states xmi:type="State" xmi:id="_mIKfMM0EEeepstnKHbY02Q" id="Recovered" uuid="_mIKfMc0EEeepstnKHbY02Q"/>
    <states xmi:type="FinalState" xmi:id="_nuzs4M0EEeepstnKHbY02Q" id="Final State 3" uuid="_nu0T8M0EEeepstnKHbY02Q"/>
    <transitions xmi:type="Transition" xmi:id="_pKMFwM0EEeepstnKHbY02Q" from="_jhv2kM0EEeepstnKHbY02Q" to="_kBAlcM0EEeepstnKHbY02Q" id="Transition 4" uuid="_pKMs0M0EEeepstnKHbY02Q"/>
    <transitions xmi:type="Transition" xmi:id="_pmiDgM0EEeepstnKHbY02Q" from="_kBAlcM0EEeepstnKHbY02Q" to="_lBmfwM0EEeepstnKHbY02Q" triggerType="message" messageCheckerType="equals" messageCheckerClass="Object" messageCheckerCode="return SIRStateChartAgent.CONTACT_INFECTED;" id="Transition 5" guard="double r = RandomHelper.getUniform().nextDouble();&#xA;return r &lt;= agent.getInfectionProb();" guardImports="import repast.simphony.random.RandomHelper;" uuid="_pmiDgc0EEeepstnKHbY02Q"/>
    <transitions xmi:type="Transition" xmi:id="_p-fQgM0EEeepstnKHbY02Q" from="_lBmfwM0EEeepstnKHbY02Q" to="_mIKfMM0EEeepstnKHbY02Q" triggerType="exponential" messageCheckerClass="Object" id="Transition 6" triggerExpRateCode="agent.getIllnessDuration();" triggerExpRateCodeImports="import sir.SIRStateChartAgent;" uuid="_p-fQgc0EEeepstnKHbY02Q"/>
    <transitions xmi:type="Transition" xmi:id="_qem7AM0EEeepstnKHbY02Q" from="_mIKfMM0EEeepstnKHbY02Q" to="_nuzs4M0EEeepstnKHbY02Q" triggerType="condition" triggerConditionCode="false;" messageCheckerClass="Object" id="Transition 7" uuid="_qem7Ac0EEeepstnKHbY02Q"/>
    <transitions xmi:type="Transition" xmi:id="_WRM7oM0GEeepstnKHbY02Q" from="_kBAlcM0EEeepstnKHbY02Q" to="_kBAlcM0EEeepstnKHbY02Q" onTransition="agent.makeContact();" triggerType="exponential" messageCheckerClass="Object" id="Transition 62" triggerExpRateCode="return agent.getContactRate();" uuid="_WROw0M0GEeepstnKHbY02Q" selfTransition="true"/>
  </StateMachine>
  <notation:Diagram xmi:id="_iGq_sM0EEeepstnKHbY02Q" type="Statechart" element="_iFFEQM0EEeepstnKHbY02Q" name="statechart.rsc" measurementUnit="Pixel">
    <children xmi:type="notation:Shape" xmi:id="_jl_dYM0EEeepstnKHbY02Q" type="2007" element="_jhv2kM0EEeepstnKHbY02Q" fontName="Segoe UI">
      <layoutConstraint xmi:type="notation:Bounds" xmi:id="_jl_dYc0EEeepstnKHbY02Q" x="525" y="198"/>
    </children>
    <children xmi:type="notation:Shape" xmi:id="_kBKWcM0EEeepstnKHbY02Q" type="2003" element="_kBAlcM0EEeepstnKHbY02Q" fontName="Segoe UI">
      <children xmi:type="notation:DecorationNode" xmi:id="_kBNZwM0EEeepstnKHbY02Q" type="5001"/>
      <layoutConstraint xmi:type="notation:Bounds" xmi:id="_kBKWcc0EEeepstnKHbY02Q" x="498" y="240"/>
    </children>
    <children xmi:type="notation:Shape" xmi:id="_lBpjEM0EEeepstnKHbY02Q" type="2003" element="_lBmfwM0EEeepstnKHbY02Q" fontName="Segoe UI">
      <children xmi:type="notation:DecorationNode" xmi:id="_lBqxMM0EEeepstnKHbY02Q" type="5001"/>
      <layoutConstraint xmi:type="notation:Bounds" xmi:id="_lBpjEc0EEeepstnKHbY02Q" x="507" y="312"/>
    </children>
    <children xmi:type="notation:Shape" xmi:id="_mIMUYM0EEeepstnKHbY02Q" type="2003" element="_mIKfMM0EEeepstnKHbY02Q" fontName="Segoe UI">
      <children xmi:type="notation:DecorationNode" xmi:id="_mIM7cM0EEeepstnKHbY02Q" type="5001"/>
      <layoutConstraint xmi:type="notation:Bounds" xmi:id="_mIMUYc0EEeepstnKHbY02Q" x="501" y="384"/>
    </children>
    <children xmi:type="notation:Shape" xmi:id="_nu1iEM0EEeepstnKHbY02Q" type="2008" element="_nuzs4M0EEeepstnKHbY02Q" fontName="Segoe UI">
      <layoutConstraint xmi:type="notation:Bounds" xmi:id="_nu1iEc0EEeepstnKHbY02Q" x="530" y="456"/>
    </children>
    <styles xmi:type="notation:DiagramStyle" xmi:id="_iGrmwM0EEeepstnKHbY02Q"/>
    <edges xmi:type="notation:Edge" xmi:id="_pKV2wM0EEeepstnKHbY02Q" type="4001" element="_pKMFwM0EEeepstnKHbY02Q" source="_jl_dYM0EEeepstnKHbY02Q" target="_kBKWcM0EEeepstnKHbY02Q">
      <styles xmi:type="notation:RoutingStyle" xmi:id="_pKV2wc0EEeepstnKHbY02Q"/>
      <styles xmi:type="notation:FontStyle" xmi:id="_pKV2ws0EEeepstnKHbY02Q" fontName="Segoe UI"/>
      <bendpoints xmi:type="notation:RelativeBendpoints" xmi:id="_pKV2w80EEeepstnKHbY02Q" points="[0, 0, 1, -52]$[1, 32, 2, -20]"/>
      <sourceAnchor xmi:type="notation:IdentityAnchor" xmi:id="_pK53cM0EEeepstnKHbY02Q" id="CENTER"/>
    </edges>
    <edges xmi:type="notation:Edge" xmi:id="_pmjRoM0EEeepstnKHbY02Q" type="4001" element="_pmiDgM0EEeepstnKHbY02Q" source="_kBKWcM0EEeepstnKHbY02Q" target="_lBpjEM0EEeepstnKHbY02Q">
      <styles xmi:type="notation:RoutingStyle" xmi:id="_pmj4sM0EEeepstnKHbY02Q"/>
      <styles xmi:type="notation:FontStyle" xmi:id="_pmj4sc0EEeepstnKHbY02Q" fontName="Segoe UI"/>
      <bendpoints xmi:type="notation:RelativeBendpoints" xmi:id="_pmj4ss0EEeepstnKHbY02Q" points="[-1, 9, 0, -37]$[-1, 41, 0, -5]"/>
      <sourceAnchor xmi:type="notation:IdentityAnchor" xmi:id="_pmlt4M0EEeepstnKHbY02Q" id="(0.5342465753424658,0.775)"/>
      <targetAnchor xmi:type="notation:IdentityAnchor" xmi:id="_pmlt4c0EEeepstnKHbY02Q" id="(0.5178571428571429,0.125)"/>
    </edges>
    <edges xmi:type="notation:Edge" xmi:id="_p-geoM0EEeepstnKHbY02Q" type="4001" element="_p-fQgM0EEeepstnKHbY02Q" source="_lBpjEM0EEeepstnKHbY02Q" target="_mIMUYM0EEeepstnKHbY02Q">
      <styles xmi:type="notation:RoutingStyle" xmi:id="_p-geoc0EEeepstnKHbY02Q"/>
      <styles xmi:type="notation:FontStyle" xmi:id="_p-geos0EEeepstnKHbY02Q" fontName="Segoe UI"/>
      <bendpoints xmi:type="notation:RelativeBendpoints" xmi:id="_p-geo80EEeepstnKHbY02Q" points="[-3, 20, -3, -52]$[-3, 52, -3, -20]"/>
    </edges>
    <edges xmi:type="notation:Edge" xmi:id="_qeowMM0EEeepstnKHbY02Q" type="4001" element="_qem7AM0EEeepstnKHbY02Q" source="_mIMUYM0EEeepstnKHbY02Q" target="_nu1iEM0EEeepstnKHbY02Q">
      <styles xmi:type="notation:RoutingStyle" xmi:id="_qeowMc0EEeepstnKHbY02Q"/>
      <styles xmi:type="notation:FontStyle" xmi:id="_qeowMs0EEeepstnKHbY02Q" fontName="Segoe UI"/>
      <bendpoints xmi:type="notation:RelativeBendpoints" xmi:id="_qeowM80EEeepstnKHbY02Q" points="[1, 6, -2, -39]$[3, 38, 0, -7]"/>
      <sourceAnchor xmi:type="notation:IdentityAnchor" xmi:id="_qerMcM0EEeepstnKHbY02Q" id="(0.4852941176470588,0.85)"/>
      <targetAnchor xmi:type="notation:IdentityAnchor" xmi:id="_qerzgM0EEeepstnKHbY02Q" id="NORTH"/>
    </edges>
    <edges xmi:type="notation:Edge" xmi:id="_WRP-8M0GEeepstnKHbY02Q" type="4001" element="_WRM7oM0GEeepstnKHbY02Q" source="_kBKWcM0EEeepstnKHbY02Q" target="_kBKWcM0EEeepstnKHbY02Q">
      <styles xmi:type="notation:RoutingStyle" xmi:id="_WRP-8c0GEeepstnKHbY02Q"/>
      <styles xmi:type="notation:FontStyle" xmi:id="_WRP-8s0GEeepstnKHbY02Q" fontName="Segoe UI"/>
      <bendpoints xmi:type="notation:RelativeBendpoints" xmi:id="_WRP-880GEeepstnKHbY02Q" points="[-65, 36, -33, 20]$[-68, 24, -36, 8]"/>
      <sourceAnchor xmi:type="notation:IdentityAnchor" xmi:id="_WRR0IM0GEeepstnKHbY02Q" id="(0.9315068493150684,0.1)"/>
    </edges>
  </notation:Diagram>
</xmi:XMI>
