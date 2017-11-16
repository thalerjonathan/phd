<?xml version="1.0" encoding="UTF-8"?>
<xmi:XMI xmi:version="2.0" xmlns:xmi="http://www.omg.org/XMI" xmlns="http://repast.sf.net/statecharts" xmlns:notation="http://www.eclipse.org/gmf/runtime/1.0.2/notation">
  <StateMachine xmi:id="_bGDiAcqmEee3cuB2xVrVAA" agentType="socialForce.scenario.pillarHall.Person" package="socialForce.chart.pillarHall" className="PersonHallStatechart" nextID="42" id="Person Hall Statechart" uuid="_bGDiAMqmEee3cuB2xVrVAA">
    <states xmi:type="PseudoState" xmi:id="_fRN64MqmEee3cuB2xVrVAA" id="Entry State Pointer" type="entry"/>
    <states xmi:type="State" xmi:id="_haBacMqmEee3cuB2xVrVAA" id="goingToEntrance" onEnter="agent.destToEntry();" uuid="_haCBgMqmEee3cuB2xVrVAA"/>
    <states xmi:type="State" xmi:id="_6HdIEMqnEee3cuB2xVrVAA" id="moving" onEnter="Point p = new Point();&#xA;while(true){&#xA;&#x9;//System.out.println(p.getY());&#xA;&#x9;p = get_Main().area.randomPointInside();&#xA;&#x9;if(roomNo==0){&#xA;&#x9;&#x9;if(p.getY()&lt;get_Main().adaptiveWall.y-20){&#xA;&#x9;&#x9;&#x9;break;&#xA;&#x9;&#x9;}&#xA;&#x9;}&#xA;&#x9;if(roomNo==1){&#xA;&#x9;&#x9;if(p.getY()>get_Main().adaptiveWall.y+20){&#xA;&#x9;&#x9;&#x9;break;&#xA;&#x9;&#x9;}&#xA;&#x9;}&#xA;}&#xA;if(belongedGroup!=null){&#xA;&#x9;if(belongedGroup.modified == false){&#xA;&#x9;&#x9;belongedGroup.destX = p.getX() / meter2px;&#xA;&#x9;&#x9;belongedGroup.destY = p.getY() / meter2px;&#xA;&#x9;&#x9;belongedGroup.exit = uniform(0,1)&lt;get_Main().exitRate;&#xA;&#x9;&#x9;belongedGroup.modified = true;&#xA;&#x9;}&#xA;&#x9;destX = belongedGroup.destX;&#xA;&#x9;destY = belongedGroup.destY;&#xA;}else{&#xA;&#x9;destX = p.getX() / meter2px;&#xA;&#x9;destY = p.getY() / meter2px;&#xA;}" uuid="_6HinoMqnEee3cuB2xVrVAA"/>
    <transitions xmi:type="Transition" xmi:id="_il1tUMqmEee3cuB2xVrVAA" from="_haBacMqmEee3cuB2xVrVAA" to="_haBacMqmEee3cuB2xVrVAA" onTransition="agent.destToEntry();" triggerTime="0.02" messageCheckerClass="Object" id="Transition 1" uuid="_il1tUcqmEee3cuB2xVrVAA" selfTransition="true"/>
    <transitions xmi:type="Transition" xmi:id="_jTJfQMqmEee3cuB2xVrVAA" from="_fRN64MqmEee3cuB2xVrVAA" to="_haBacMqmEee3cuB2xVrVAA" id="Transition 2" uuid="_jTKGUMqmEee3cuB2xVrVAA"/>
    <transitions xmi:type="Transition" xmi:id="_9DVDUMqnEee3cuB2xVrVAA" from="_haBacMqmEee3cuB2xVrVAA" to="_6HdIEMqnEee3cuB2xVrVAA" onTransition="agent.resetAtDest();" triggerType="condition" triggerConditionCode="agent.isAtDest();" messageCheckerClass="Object" id="Transition 25" uuid="_9DVDUcqnEee3cuB2xVrVAA"/>
  </StateMachine>
  <notation:Diagram xmi:id="_bGaHUMqmEee3cuB2xVrVAA" type="Statechart" element="_bGDiAcqmEee3cuB2xVrVAA" name="statechart.rsc" measurementUnit="Pixel">
    <children xmi:type="notation:Shape" xmi:id="_fRu4QMqmEee3cuB2xVrVAA" type="2007" element="_fRN64MqmEee3cuB2xVrVAA" fontName="Segoe UI">
      <layoutConstraint xmi:type="notation:Bounds" xmi:id="_fRu4QcqmEee3cuB2xVrVAA" x="506" y="240"/>
    </children>
    <children xmi:type="notation:Shape" xmi:id="_haFr4MqmEee3cuB2xVrVAA" type="2003" element="_haBacMqmEee3cuB2xVrVAA" fontName="Segoe UI">
      <children xmi:type="notation:DecorationNode" xmi:id="_haG6AMqmEee3cuB2xVrVAA" type="5001"/>
      <layoutConstraint xmi:type="notation:Bounds" xmi:id="_haFr4cqmEee3cuB2xVrVAA" x="468" y="300"/>
    </children>
    <children xmi:type="notation:Shape" xmi:id="_6Hlq8MqnEee3cuB2xVrVAA" type="2003" element="_6HdIEMqnEee3cuB2xVrVAA" fontName="Segoe UI">
      <children xmi:type="notation:DecorationNode" xmi:id="_6Hm5EMqnEee3cuB2xVrVAA" type="5001"/>
      <layoutConstraint xmi:type="notation:Bounds" xmi:id="_6Hlq8cqnEee3cuB2xVrVAA" x="493" y="384"/>
    </children>
    <styles xmi:type="notation:DiagramStyle" xmi:id="_bGaHUcqmEee3cuB2xVrVAA"/>
    <edges xmi:type="notation:Edge" xmi:id="_il4JkMqmEee3cuB2xVrVAA" type="4001" element="_il1tUMqmEee3cuB2xVrVAA" source="_haFr4MqmEee3cuB2xVrVAA" target="_haFr4MqmEee3cuB2xVrVAA">
      <styles xmi:type="notation:RoutingStyle" xmi:id="_il4JkcqmEee3cuB2xVrVAA"/>
      <styles xmi:type="notation:FontStyle" xmi:id="_il4JksqmEee3cuB2xVrVAA" fontName="Segoe UI"/>
      <bendpoints xmi:type="notation:RelativeBendpoints" xmi:id="_il4Jk8qmEee3cuB2xVrVAA" points="[0, 0, 0, 0]$[0, 0, 0, 0]"/>
      <sourceAnchor xmi:type="notation:IdentityAnchor" xmi:id="_il7M4MqmEee3cuB2xVrVAA" id="(0.0,0.0)"/>
      <targetAnchor xmi:type="notation:IdentityAnchor" xmi:id="_il7M4cqmEee3cuB2xVrVAA" id="(0.0,0.0)"/>
    </edges>
    <edges xmi:type="notation:Edge" xmi:id="_jTLUcMqmEee3cuB2xVrVAA" type="4001" element="_jTJfQMqmEee3cuB2xVrVAA" source="_fRu4QMqmEee3cuB2xVrVAA" target="_haFr4MqmEee3cuB2xVrVAA">
      <styles xmi:type="notation:RoutingStyle" xmi:id="_jTLUccqmEee3cuB2xVrVAA"/>
      <styles xmi:type="notation:FontStyle" xmi:id="_jTLUcsqmEee3cuB2xVrVAA" fontName="Segoe UI"/>
      <bendpoints xmi:type="notation:RelativeBendpoints" xmi:id="_jTLUc8qmEee3cuB2xVrVAA" points="[0, 0, -1, -53]$[0, 50, -1, -3]"/>
      <sourceAnchor xmi:type="notation:IdentityAnchor" xmi:id="_jTNwsMqmEee3cuB2xVrVAA" id="CENTER"/>
      <targetAnchor xmi:type="notation:IdentityAnchor" xmi:id="_jTNwscqmEee3cuB2xVrVAA" id="(0.47572815533980584,0.075)"/>
    </edges>
    <edges xmi:type="notation:Edge" xmi:id="_9DWRcMqnEee3cuB2xVrVAA" type="4001" element="_9DVDUMqnEee3cuB2xVrVAA" source="_haFr4MqmEee3cuB2xVrVAA" target="_6Hlq8MqnEee3cuB2xVrVAA">
      <styles xmi:type="notation:RoutingStyle" xmi:id="_9DWRccqnEee3cuB2xVrVAA"/>
      <styles xmi:type="notation:FontStyle" xmi:id="_9DWRcsqnEee3cuB2xVrVAA" fontName="Segoe UI"/>
      <bendpoints xmi:type="notation:RelativeBendpoints" xmi:id="_9DWRc8qnEee3cuB2xVrVAA" points="[-8, 20, -9, -64]$[-8, 64, -9, -20]"/>
    </edges>
  </notation:Diagram>
</xmi:XMI>
