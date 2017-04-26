/*
 * The Sugarscape package is the main package and encompasses all elements 
 * of the Sugarscape project.
 */
package Sugarscape;

import javax.swing.*;
import java.awt.Graphics;
import java.awt.Event;
import java.awt.Toolkit;
import java.awt.Color;
import java.awt.Font;
import java.util.*;
import java.io.*;
import java.lang.Math;

/**
 * Implements the CellSpace object that defines the Sugarscape grid.
 * Implements grid-level methods of the project.
 * Implements data collection and display methods for results of the 
 * executing simulation.
 * CellSpace encompasses the Cell object. Also contains the 
 * Graphics objects that is used to draw the Sugarspace display
 * 
 * @author	abraham kannankeril
 */
public class CellSpace extends JPanel
{
	int population;
	Cell cell[][];

	static long gatherTime=0, mateTime=0, barterTime=0, cultureTime =0, otherTime=0,
	startTime = System.currentTimeMillis();
	static int destinationI = -1, destinationJ = -1;
	static int born = 0, dead = 0;

	private boolean cellUnderMouse;
	private boolean cells[][];
	private int cellsBuffer[][];
	//	private Image offScreenImage = null;
	private Graphics offScreenGraphics;

	private int cellCols, cellRows, cellSize;
	private Citizen citizen, mate, trader;
	public java.util.List zenList, mateList, deathList, birthList, traderList;//, germList;
	private int birthCount=0, deathByStarvation=0, deathByDotage=0, deathByDerliction=0,
		totVision=0, totSugar=0, totSuMetab=0, totSpice=0, totSpMetab=0, totLifespan=0,
		totAge=0, distrSummer=0, distrWinter=0, distrMale=0, distrFemale=0, 
		distrChildren=0, distrAdults=0, distrSeniors=0, distrHyp=0, distrHypSu=0, 
		distrHypSp=0, distrEff=0, distrIneff=0, distrSlowSu=0, distrSlowSp=0, distrSlow=0, 
		distrRiskTaker=0, distrRiskAverse=0;
	/**Cultural Grouping variable(s)
	 * - invoked by setDistrGroups() 
	 */	
	public int groupCount = GoLconst.groupCount();
	public int rangeArr[] = new int[groupCount];
	public int rangeTally[] = new int[groupCount];
	private String rangeName[] = GoLconst.CITIZEN_GROUP_NAME;
		

	/**
	 * Used to display simulation data and command feedback to the user. */
	public JTextArea textArea;
	static String msgStr = "";
	static BufferedOutputStream bos;
	public final int BIRTHEVENT = 1, DEATHEVENT = 0;

	/**
	 * CellSpace constructor - resizes physical dimensions for the Sugarscape
	 * Defines Linked list objects that contain the citizen population.
	 * Defines the Cell objects that make the grid.
	 * 
	 * @param	cellSize
	 * Defines the size of each individual cell.
	 * @param	cellCols
	 * Defines the number of columns on the grid.
	 * @param	cellRows
	 * Defines the number of rows on the grid.
	 * @param	textArea
	 * Handle to the text area for display of program feedback.
	 */
	public CellSpace(  int cellSize, int cellCols, int cellRows, JTextArea textArea )
	{	//super(new BorderLayout());
	
		/** boolean grid indicating presence of citizen - used mostly in GameOfLife code */
		cells			= new boolean[GoLconst.GRID_COLS_MAX][GoLconst.GRID_ROWS_MAX];
		cellsBuffer		= new int[GoLconst.GRID_COLS_MAX][GoLconst.GRID_ROWS_MAX];
		/** grid containing Cell objects - used exclusively in Sugarscape code */
		cell			= new Cell[GoLconst.GRID_COLS_MAX][GoLconst.GRID_ROWS_MAX];

		this.cellSize = cellSize;
		this.cellCols = cellCols;
		this.cellRows = cellRows;
		this.textArea = textArea;

		//---Creating list for citizens
		zenList = new LinkedList();		//main list comprising all citizens on Ss
		mateList = new LinkedList();	//list of available mates for curr citizen
		//germList = new LinkedList();	//set of diseases prevalent on the Ss

		//reset grid dimensions based on parameters passed
		//reshape(0, 0, cellSize*cellCols-1, cellSize*cellRows-1);
	}

	/**
	 * Returns the current cycle or time period of the simulation under 
	 * execution. The variable is accessed from the global constants file,
	 * GoLconst.java
	 * 
	 * @return	GoLconst.timePeriod
	 * @see	GoLconst#timePeriod
	 */
	public int getTimePeriod()
	{	return GoLconst.timePeriod;	}
	/**
	 * Resets the time period for a new round of the simulation
	 * 
	 */
	public void resetTimePeriod()
	{	GoLconst.timePeriod = 0;	}
	
	/**
	 * Increments time period by one
	 * 
	 * @return	new timeperiod
	 */
	public int incrementTime()
	{	return ++GoLconst.timePeriod;	}

	/**
	 * Reports the current population size.
	 * 
	 * @return	number of citizens
	 */
	public int getPopulation()
	{	return zenList.size();	}

	/**
	 * Creates instances of the Cell object. The number of instances are 
	 * determined by the product of the number of columns and rows.
	 * 
	 */
	public void createCells()
	{	int cntr = 0;
		
		for(int i = 0; i < cellCols; i++)
			for(int j = 0; j < cellRows; j++)
				if( cell[i][j] == null )
				{	cntr++;
					cell[i][j] = new Cell();
				}
		if( GoLconst.DEBUG_CMD_FEEDBACK )
			textArea.append("New cells added: " + cntr + "\n");
	}


	/**
	 * Creates new child. 
	 * If the first two parameters are null, then the child object properties are 
	 * randomly derived.
	 * If citizen objects are passed in the first two parameters, the new child
	 * inherits its properties from its parents
	 */
	public Citizen createCitizen(Citizen citizen, Citizen mate, int col, int row)
	{
		cells[col][row] = true;
		if(citizen == null)
		{	cell[col][row].citizen =  new Citizen(col,row); } 
		else
		{	cell[col][row].citizen =  new Citizen(citizen, mate, col,row);
			//Change Parent Properties to Reflect Child Birth
			citizen.setLastMating(getTimePeriod());
			citizen.addChild( cell[col][row].citizen );
			mate.setLastMating(getTimePeriod());
			mate.addChild( cell[col][row].citizen );
		}
			
		//Gather sugar & spice at current location
		if( cell[col][row].getSugar() > 0 )
		{	cell[col][row].citizen.setSugar(
					(cell[col][row].citizen.getInheritSugar()
					+ cell[col][row].getSugar()) );
			cell[col][row].setSugar(0f);		//also adjusts pollution
		}
		if( cell[col][row].getSpice() > 0 )
		{	cell[col][row].citizen.setSpice(
					(cell[col][row].citizen.getInheritSpice()
					+ cell[col][row].getSpice()) );
			cell[col][row].setSpice(0f);
		}
	
		setTotSugarSpice(cell[col][row].citizen, 1);
		cell[col][row].citizen.setSpUnits();
		cell[col][row].citizen.setSuUnits();
		cell[col][row].citizen.setTUD();
	
		zenList.add( cell[col][row].citizen );
		population++;
		setCellspaceStats(cell[col][row].citizen, BIRTHEVENT);
	
	
		if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_BIRTH )
		{	cell[col][row].citizen.showStats("Born! ");	}	
		
		return cell[col][row].citizen;
	}


	/**
	 * Populates cells in the Sugarscape based on a frequency determined by the
	 * global constant DENSITY_FACTOR in GoLconst.java.
	 * 
	 * @return	the new population size.
	 * @see	GoLconst#DENSITY_FACTOR
	 */
	public int populate()
	{	Citizen citizen;
		for( int i=0; i < cellCols; i++ )
		{	for( int j=0; j < cellRows; j++ )
			{	if( Math.random() < GoLconst.DENSITY_FACTOR )
				{	cell[i][j].citizen = new Citizen(i,j);
					citizen = cell[i][j].citizen;
					//Set new citizen's sugar to inherited sugar + curr cell sugar
					if( cell[i][j].getSugar() > 0 )
					{	citizen.setSugar(citizen.getSugar() + cell[i][j].getSugar());
						cell[i][j].setSugar(0f);	//also adjusts pollution
					}
					if( cell[i][j].getSpice() > 0 )
					{	citizen.setSpice(citizen.getSpice()	+ cell[i][j].getSpice());
						cell[i][j].setSpice(0f);
					}
					zenList.add(citizen);
					population++;
					setCellspaceStats(cell[i][j].citizen, BIRTHEVENT);
				}
			}
		}
		setDistrGroups();	//incorporate into setCellspeceStats()
		return population;
	}

	/**
	 * Generate initial/periodic distribution of sugar in Cellspace when
	 * seasonal processes inactive.
	 * 
	 */
	public void growGoods()
	{	
		float cellSugar, cellSpice;
		for(int i = 0; i < cellCols; i++)
		{	for(int j = 0; j < cellRows; j++)
			{	cellSugar = cell[i][j].getSugar();
				cellSugar = cellSugar > 0 ? cellSugar : 0;  
				cellSpice = cell[i][j].getSpice();
				cellSpice = cellSpice > 0 ? cellSpice : 0;
				
				if( getTimePeriod() > 0 )
				{	if( cellSugar < 0 )	//Cell Incapable of Growing Sugar
						break;
					//Renew Available Sugar up to Max cell capacity
					else if( GoLconst.LIMIT_CELL_SUGAR )
						cell[i][j].setSugar( cellSugar +
								(cellSugar < GoLconst.SUGAR_MAX_CELL ?
											GoLconst.SUGAR_RENEW_SUMMER : 0) );
					else
						cell[i][j].setSugar(cellSugar + GoLconst.SUGAR_RENEW_SUMMER);

					//renew available spice up to max cell capacity
					if( cellSpice < 0 )
						break;		//cell incapable of growing spice
					else if( GoLconst.LIMIT_CELL_SPICE )
						cell[i][j].setSpice( cellSpice + 
								(cellSpice < GoLconst.SPICE_MAX_CELL ?
											GoLconst.SPICE_RENEW_SUMMER : 0) );
					else
						cell[i][j].setSpice( cellSpice + GoLconst.SPICE_RENEW_SUMMER );
				}
				else	//Initializing Cells for First time
				{	cell[i][j].setSugar( -1f );
					cell[i][j].setSpice( -1f );
				}
			}
		}
	}

	/**
	 * Generate initial/periodic distribution of sugar in Cellspace when
	 * seasonal processes have been activated.
	 */
	public void growGoodsBySeason()
	{	float cellSugar, cellSpice;
		String area1 = ((getTimePeriod() / GoLconst.SEASON_DURATION) % 2) == 0 ?
								"SUMMER" : "WINTER";
		int border	= cellRows / 2;
		int j;

		for(int i = 0; i < cellCols; i++)
		{	for(j = 0; j < border; j++)	//renewing SUMMER section of grid
			{	cellSugar = cell[i][j].getSugar();
				cellSugar = cellSugar > 0 ? cellSugar : 0;  
				cellSpice = cell[i][j].getSpice();
				cellSpice = cellSpice > 0 ? cellSpice : 0;

				if(  getTimePeriod() > 0 )
				{	if( cellSugar < 0 )		//cell incapable of growing sugar
						break;
					else if( GoLconst.LIMIT_CELL_SUGAR )
					{	if( cellSugar < GoLconst.SUGAR_MAX_CELL )
						{	if( area1.equals("SUMMER") )
								cell[i][j].setSugar( cellSugar 
											+ GoLconst.SUGAR_RENEW_SUMMER );
							else
								cell[i][j].setSugar( cellSugar 
											+ GoLconst.SUGAR_RENEW_WINTER );
						}
					}
					else
					{	cell[i][j].setSugar( cellSugar + (area1.equals("SUMMER")?
							GoLconst.SUGAR_RENEW_SUMMER : GoLconst.SUGAR_RENEW_WINTER) );
					}

					if( cellSpice < 0 )
						break;		//cell incapable of growing spice
					else if( GoLconst.LIMIT_CELL_SPICE )
					{	if( cellSpice < GoLconst.SPICE_MAX_CELL )
						{
							if( area1.equals("SUMMER") )
								cell[i][j].setSpice( cellSpice 
											+ GoLconst.SPICE_RENEW_SUMMER );
							else
								cell[i][j].setSpice( cellSpice 
											+ GoLconst.SPICE_RENEW_WINTER );
						}
					}
					else
					{	cell[i][j].setSpice( cellSpice + (area1.equals("SUMMER")?
							GoLconst.SPICE_RENEW_SUMMER : GoLconst.SPICE_RENEW_WINTER) );
					}
				}
				else	//initializing cell sugar & spice for the first time
				{	cell[i][j].setSugar( -1 );
					cell[i][j].setSpice( -1 );
				}
			}

			for( ; j < cellRows; j++)	//renewing WINTER section of grid
			{	cellSugar = cell[i][j].getSugar();
				cellSugar = cellSugar > 0 ? cellSugar : 0;  
				cellSpice = cell[i][j].getSpice();
				cellSpice = cellSpice > 0 ? cellSpice : 0;

				if(  getTimePeriod() > 0 )
				{	if( cellSugar < 0 )	//cell incapable of growing sugar
						break;
					else if( GoLconst.LIMIT_CELL_SUGAR )
					{	if( cellSugar < GoLconst.SUGAR_MAX_CELL )
						{	if( area1.equals("WINTER") )
								cell[i][j].setSugar( cellSugar 
											+ GoLconst.SUGAR_RENEW_SUMMER );
							else
								cell[i][j].setSugar( cellSugar 
											+ GoLconst.SUGAR_RENEW_WINTER );
						}
					}
					else
					{	cell[i][j].setSugar( cellSugar + (area1.equals("WINTER")?
							GoLconst.SUGAR_RENEW_SUMMER : GoLconst.SUGAR_RENEW_WINTER) );
					}

					if( cellSpice < 0 )
						break;		//cell incapable of growing spice
					else if( GoLconst.LIMIT_CELL_SPICE )
					{	if( cellSpice < GoLconst.SPICE_MAX_CELL )
						{
							if( area1.equals("WINTER") )
								cell[i][j].setSpice( cellSpice 
											+ GoLconst.SPICE_RENEW_SUMMER );
							else
								cell[i][j].setSpice( cellSpice 
											+ GoLconst.SPICE_RENEW_WINTER );
						}
					}
					else
					{	cell[i][j].setSpice( cellSpice + (area1.equals("WINTER")?
							GoLconst.SPICE_RENEW_SUMMER : GoLconst.SPICE_RENEW_WINTER) );
					}
				}
				else	//initializing cell sugar & spice for the first time
				{	cell[i][j].setSugar( -1 );
					cell[i][j].setSpice( -1 );
				}
			}
		}
	}


	
	/**
	 * Periodic dispersal of accumulated pollution caused by gathering activity.
	 * 
	 */
	public  void dispersePollution()
	{	for(int i=0; i<cellCols; i++)
		{	for(int j=0; j<cellRows; j++)
				cell[i][j].setPollution( 0 );
		}
	}

	/**
	 * Calculates pollutant accumulated in cells on the grid.
	 * 
	 * @return	total accumulated pollutant on the grid.
	 */
	public  float sumPollutant()
	{	float sumPollutant = 0f;
		for(int i=0; i<cellCols; i++)
			for(int j=0; j<cellRows; j++)
				sumPollutant += cell[i][j].getPollution();
		return sumPollutant;
	}

	/**
	 * Eliminates citizens marooned by dynamic reduction in grid dimension.
	 * 
	 * @param	newCellCols	new value for number of columns on the grid.
	 * @param	newCellRows	new value for number of rows on the grid.
	 * @return	number of citizens eliminated by resizing the grid.
	 */
	public  int deleteOrphanCitizens(int newCellCols, int newCellRows)
	{
		int index = 0, count = 0;
		int col, row;
		Citizen citizen;
		deathList = new LinkedList();

		int zenCount = zenList.size();
		while(  index < zenCount  )
		{	
			citizen = (Citizen) zenList.get(index++);
			col = citizen.getCol();
			row = citizen.getRow();
			//Check if citizen is outside current grid boundaries
			if( col >= newCellCols || row >= newCellRows )
			{
				if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_DEATH)
				{	citizen.showStats("Died-Marooned! ");	}
				citizen.setCauseOfDeath("Marooned");
				deathList.add( cell[col][row].citizen );
				population-- ;
				deathByDerliction++;
				setCellspaceStats(cell[col][row].citizen, DEATHEVENT);
				count++;
				cell[col][row].citizen = null;
			}
		}
		zenList.removeAll(deathList);
		return count;
	}
	
	/**
	 * Initiates change in the number of columns on the grid.
	 * 
	 * @param	cols	the new value for columns in the grid.
	 */
	public  void resizeCols(int cols)
	{	this.cellCols = cols;
		GoLconst.GRID_COLUMNS = cols;
	}
	
	/**
	 * Initiates change in the number of rows on the grid.
	 * 
	 * @param	rows	the new value for rows in the grid.
	 */
	public  void resizeRows(int rows)
	{	this.cellRows = rows;
		GoLconst.GRID_ROWS = rows;
	}

	/**
	 * Calls routine to toggle current cell status. If occupied, eliminates
	 * citizen or creates new citizen otherwise. 
	 * 
	 * @param	evt	event handle passed by system, not used.
	 * @param	x	x-axis parameter for pixel.
	 * @param	y	y-axis parameter for pixel.
	 * @return	always returns true
	 */
	public  boolean mouseUp(Event evt, int x, int y) {
		toggleCellUnderMouse( x,y );
		return true;
	}

	/**
	 * Cell selection operation<BR>
	 * Copies status of cell selected by the user. 
	 * If occupied, eliminates citizen or creates new citizen otherwise. 
	 * 
	 * @param	evt
	 * @param	x
	 * @param	y
	 * @return	always returns true
	 */
	public  boolean mouseDown(Event evt, int x, int y)
	{	try { cellUnderMouse = cells[x/cellSize][y/cellSize];
		} catch ( java.lang.ArrayIndexOutOfBoundsException e ) {}
		return true;
	}

	/**
	 * Calls routine to toggle status of a range of cells selected by dragging 
	 * the mouse. If occupied, eliminates citizen, creates new citizen otherwise. 
	 * 
	 * @param	evt
	 * @param	x
	 * @param	y
	 */
	public  boolean mouseDrag(Event evt, int x, int y)
	{	toggleCellUnderMouse( x,y );
		return true;
	}

	/**
	 * Called by the mouseUp & mouseDrag methods. Does the actual work marking 
	 * a citizen for elimination if the cell is occupied or creating a new 
	 * citizen in the cell when it is empty.
	 * 
	 * @param	x	identifies the column where the cell is located.
	 * @param	y	identifies the row where the cell is located.
	 * @return	True, upon completion of operation.
	 */
	public  boolean toggleCellUnderMouse(int x, int y)
	{	//Determine Col from pixel position on grid
		int col = x/cellSize, row = y/cellSize;

		// toggle cell
		try { cells[col][row] = !cellUnderMouse; }
		catch ( java.lang.ArrayIndexOutOfBoundsException e ) {}

		//Cell toggled ON - create new citizen
		if( cells[col][row] )
		{	cell[col][row].citizen = new Citizen(col,row);
			citizen = cell[col][row].citizen;
			if( cell[col][row].getSugar() > 0 )
			{	cell[col][row].citizen.setSugar( (cell[col][row].citizen.getSugar()
											+ cell[col][row].getSugar()) );
				cell[col][row].setSugar(0f); //cell emptied, set sugar to zero
			}
			if( cell[col][row].getSpice() > 0 )
			{	cell[col][row].citizen.setSpice( (cell[col][row].citizen.getSpice()
											+ cell[col][row].getSpice()) );
				cell[col][row].setSpice(0f); //cell emptied, set sugar to zero
			}
			zenList.add( citizen );
			if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_BIRTH )
			{	citizen.showStats("Born! Citizen");	}
			population++;
			setCellspaceStats(cell[col][row].citizen, BIRTHEVENT);
		} else	//Cell toggled OFF - so kill citizen
		{
			deathList.add(cell[col][row].citizen);
			setCellspaceStats(cell[col][row].citizen, DEATHEVENT);
			cell[col][row].citizen = null;
			if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_BIRTH )
				citizen.showStats("Died-Killed by You! ");
		}
		repaint();
		setVisible(true);
		return true;
	}

	/**
	 * sets OffScreenImage object to null.
	 * 
	 */
	//public  void setOffScreenImage()
	//{	offScreenImage = null;	}

	/**
	 * @param	theG	handle to the graphics object.
	 */
	public  void update( Graphics theG )
	{	//Dimension d = size();
		//if((offScreenImage == null) ) {
		//	offScreenImage = createImage( d.width, d.height );
		//	offScreenGraphics = offScreenImage.getGraphics();
		//}
		paint(offScreenGraphics);
		//theG.drawImage( offScreenImage, 0, 0, null );
	}

	/**
	 * The main paint method for (re)drawing the Sugarscape. Based on the 
	 * template group selected (Game of Life / Sugarscape), the appropriate 
	 * grid is displayed.
	 * The method then determines the status of the cell and calls either  
	 * Cell paint, if the cell is empty or the Citizen paint method.
	 * Column and row coordinates are also displayed based on options configured 
	 * in the global constants file.
	 * 
	 * @param	g	handle to the graphics object - used to draw the grid components
	 * including cells and citizens.
	 * @see	Cell#paintCell(java.awt.Graphics, int, int, int)
	 * @see	Citizen#paintZen(java.awt.Graphics, int)
	 */
	public  void paint(Graphics g)
	{	/** draw background (MSIE doesn't do that) */
		String area1 = ((getTimePeriod() / GoLconst.SEASON_DURATION) % 2) == 0 ?
				"SUMMER" : "WINTER";
		int mBgColor = GoLconst.CELL_BG_SUMMER;
		
		g.setColor( new Color(mBgColor,mBgColor,mBgColor) );//resets window bgrnd
		//font setting for grid coordinates
		g.setFont( new Font("Verdana",Font.PLAIN,
						(int)((cellSize*GoLconst.GRID_COORD_FONT) )) );
		
		
		//draws the window background for the grid
		g.fillRect( 0, 0, cellSize*cellCols-1, cellSize*cellRows-1 );

		// draw cells
		for( int x=0; x<cellCols; x++ )
		{	for( int y=0; y<cellRows; y++ )
			{	// **GameOfLife Display**
				if( !GoLconst.flagSugarscape )
				{	if ( cells[x][y] )
					{	g.setColor( Color.yellow );	}
					else
					g.fillRect( x*cellSize, y*cellSize, cellSize-1, cellSize-1 );
					continue;
				}

				//Draw Sugarscape Cell**
				if( cell[x][y].notOccupied() )
				{	cell[x][y].paintCell(g, x, y, cellSize);

					/*Write cell coordinates*/
					if( GoLconst.GRID_COORD_SHOW )
					{	if(  GoLconst.GRID_COORD_DETAIL==0 ||
							(GoLconst.GRID_COORD_DETAIL==1 && (x == cellCols-1 || y == cellRows-1)) ||
							(GoLconst.GRID_COORD_DETAIL==2 && x == cellCols-1) ||
							(GoLconst.GRID_COORD_DETAIL==3 && y == cellRows-1) ||
							(GoLconst.GRID_COORD_DETAIL==4 && x == cellCols-1 && y == cellRows-1 )
							)
						{	if( GoLconst.GRID_COORD_COLOR == "blue" )
								g.setColor( Color.blue);
							else if( GoLconst.GRID_COORD_COLOR == "red" )
								g.setColor( Color.red );
							else if( GoLconst.GRID_COORD_COLOR == "green" )
								g.setColor( Color.green );
							else g.setColor( Color.black );

							if( x == (cellCols-1) )
								g.drawString(y+"", x*cellSize, (y+1)*cellSize-1);
							else
								g.drawString(x+"", x*cellSize, (y+1)*cellSize-1);
						}
					}
				}
				else
					cell[x][y].citizen.paintZen(g, cellSize);

			}
		}

		// draw grid
		g.setColor( Color.black );
		//draw the vertical lines on the grid
		for( int x=1; x<cellCols; x++ )
		{	g.drawLine( x*cellSize-1, 0, x*cellSize-1, cellSize*cellRows-1 );	}
		//draw the horizontal lines on the grid
		for( int y=1; y<cellRows; y++ )
		{	g.drawLine( 0, y*cellSize-1, cellSize*cellCols-1, y*cellSize-1 );	}
	}

	/**
	 * Clears Grid, empties the Citizen List and (re)initializes grid vars.
	 * 
	 */
	public  void clear()
	{	resetTimePeriod();
		Citizen.resetID();
		population = 0;
		int index = 0;
		int cntr = 0;
		for( int x=0; x<cellCols; x++ )
			for( int y=0; y<cellRows; y++ )
			{	cells[x][y] = false;
                //if (cell[x][y] == null)
                //{
                    cntr++;
                    cell[x][y] = new Cell();
                //} else
                    //cell[x][y].citizen = null;
			}
			if( GoLconst.DEBUG_CMD_FEEDBACK && cntr > 0 )
			{	textArea.append("Creating cells..." + cntr + " cells created\n"); }

		//If Sugarscape, Clear Citizen List as well
		if( GoLconst.flagSugarscape )
		{	index = zenList.size();
			for(int i=index-1; i>=0; i--)
				zenList.remove(i);
		}
		//Reinitialize time vars
		gatherTime=0; mateTime=0; otherTime=0;
		startTime = System.currentTimeMillis();
		
		//Reinitialize static vars
		born = 0;		//used in showPopulationStats()
		dead = 0;		//--"--
		resetScoreVars();
	}

	/**
	 * Determines the next generation in the Game of Life templates
	 * 
	 */
	public  void next()
	{	int x;
		int y;
		incrementTime();
		// clear the buffer
		for( x=0; x<cellCols; x++ )
			for( y=0; y<cellRows; y++ )
			{	cellsBuffer[x][y] = 0;	}

		// count neighbors of off-edge cells
		for( x=1; x<cellCols-1; x++ )
			for( y=1; y<cellRows-1; y++ )
				if ( cells[x][y] )
				{	cellsBuffer[x-1][y-1]++;		cellsBuffer[x][y-1]++;
					cellsBuffer[x+1][y-1]++;		cellsBuffer[x-1][y]++;
					cellsBuffer[x+1][y]++;			cellsBuffer[x-1][y+1]++;
					cellsBuffer[x][y+1]++;			cellsBuffer[x+1][y+1]++;
				}
		// count neighbors of edge cells
		x=1; // start at (1,0)
		y=0;
		int dx=1;		int dy=0;
		while( true )
		{	if ( cells[x][y] )
			{	if ( x > 0 )
				{	if ( y > 0 )			{	cellsBuffer[x-1][y-1]++;	}
					if ( y < cellRows-1 )	{	cellsBuffer[x-1][y+1]++;	}
					cellsBuffer[x-1][y]++;
				}
				if ( x < cellCols-1 )
				{	if ( y < cellRows-1 )	{	cellsBuffer[x+1][y+1]++;	}
					if ( y > 0 )			{	cellsBuffer[x+1][y-1]++;	}
					cellsBuffer[x+1][y]++;
				}
				if ( y > 0 )				{	cellsBuffer[x][y-1]++;		}
				if ( y < cellRows-1 )		{	cellsBuffer[x][y+1]++;		}
			}
			// turn clockwise at collision with edge
			if ( x==cellCols-1 && y==0 )
			{	dx = 0;		dy = 1;		}
			else if ( x==cellCols-1 && y==cellRows-1 )
			{	dx = -1;	dy = 0;		}
			else if ( x==0 && y==cellRows-1 )
			{	dx = 0;		dy = -1;	}
			else if ( x==0 && y==0 ) // all edge cells done
			{	break;	}
			x += dx;
			y += dy;
		}
		// here is the life algorithm
        for (x = 0; x < cellCols; x++)
        {	for (y = 0; y < cellRows; y++)
            {	switch (cellsBuffer[x][y])
                {	case 2 : // no change
                        break;
                    case 3 :
                        cells[x][y] = true;
                        break;
                    default :
                        cells[x][y] = false;
                        break;
                }
            }
        }
	}

	/**
	 * Draws selected pattern for the first time in Game of Life template. 
	 * 
	 * @param	shapeWidth	Max width of the pattern to be drawn on the grid.
	 * @param	shapeHeight	Max height of the pattern to be drawn on the grid.
	 * @param	shape	Boolean array defining the pattern to be displayed on the grid.
	 * @return	returns false if shape doesn't fit within current grid dimensions.
	 */
	public  boolean drawShape( int shapeWidth, int shapeHeight, int shape[] ) {
		int xOffset;
		int yOffset;

		if ( shapeWidth>cellCols || shapeHeight>cellRows )
			return false; // shape doesn't fit on canvas

		// center the shape
		xOffset = (cellCols-shapeWidth)/2;
		yOffset = (cellRows-shapeHeight)/2;
		clear();
		for ( int i=0; i < shape.length; i+=2 )
			cells[xOffset+shape[i]][yOffset+shape[i+1]] = true;
		return true;
	}

	/**
	 * Draws random pattern for the first time in Game of Life template.
	 * 
	 * @return	True,after completion of random pattern.
	 */
	public  boolean drawRandomShape( )
	{	clear();
		for ( int i=0; i < cellCols; i++ )
			for ( int j=0; j < cellRows; j++ )
				if( ( (float)(Math.random()) ) < GoLconst.DENSITY_FACTOR)
					cells[i][j] = true;
		return true;
	}

	/**
	 * Executes simulation processes for the first time when the 
	 * Sugarscape template is selected.
	 * Populates the Sugarscape with a random distribution of citizens
	 * based on the frequency configured in the global constant
	 * DENSITY_FACTOR.
	 * 
	 * @param	cols	defines the number of columns on the Sugarscape.
	 * @param	rows	defines the number of rows on the Sugarscape.
	 * @see	Sugarscape#drawSugarscape()
	 */
	public  void drawSugarscape(int cols, int rows)
	{
		//initialize cells, population, timePeriod & reseed Sugarscape
		if( cols != cellCols || rows != cellRows )
		{	this.cellCols = cols;
			this.cellRows = rows;
			resize(this.cellCols * this.cellSize, GoLconst.CSPACE_OFFSET + 
				((cellRows) * cellSize) + (int)(GoLconst.TEXTAREA_HEIGHT * 16.67) );
		}
		clear();
		setTotAge(null, 0);
		setTotSugarSpice(null, 0);
		Citizen.setDistrGroupsInit();
		
		if(GoLconst.INITIATE_SEASONS)
			growGoodsBySeason();
		else growGoods();

		if(GoLconst.INITIATE_DISEASE)
			Citizen.generateDiseases();

		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "CellSpace.drawSugarscape()\n" );		}
		if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_BIRTH )
		{	textArea.append( "Randomly populating the grid...\n" );	}

		for(int i=0; i < cellCols; i++)
		{	for(int j=0; j < cellRows; j++)
			{
				if( (float)(Math.random()) < GoLconst.DENSITY_FACTOR)
				{	cells[i][j] = true;
					cell[i][j].citizen =  new Citizen(i,j);

					//Gather sugar & spice at current location
					if( cell[i][j].getSugar() > 0 )
					{	cell[i][j].citizen.setSugar(
								(cell[i][j].citizen.getInheritSugar()
								+ cell[i][j].getSugar()) );
						cell[i][j].setSugar(0f);		//also adjusts pollution
					}
					if( cell[i][j].getSpice() > 0 )
					{	cell[i][j].citizen.setSpice(
								(cell[i][j].citizen.getInheritSpice()
								+ cell[i][j].getSpice()) );
						cell[i][j].setSpice(0f);
					}

					setTotSugarSpice(cell[i][j].citizen, 1);
					cell[i][j].citizen.setSpUnits();
					cell[i][j].citizen.setSuUnits();
					cell[i][j].citizen.setTUD();

					zenList.add( cell[i][j].citizen );
					population++;
					setCellspaceStats(cell[i][j].citizen, BIRTHEVENT);
					

					if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_BIRTH )
					{	cell[i][j].citizen.showStats("Born! ");	}
					
				}
			}
		}
		setDistrGroups();	//incorporate into setCellspeceStats()
		repaint();
		setVisible(true);
		if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_BIRTH )
		{	textArea.append( "Grid populated.., population = " + population + "\n");	}
	}

	/**
	 * Executes simulation processes for subsequent cycles or time periods. Tracks time 
	 * spent on various broadly defined processes, list of dying and newborn citizens.
	 * Perhaps the main execution loop for the simulation.
	 * 
	 * @param	cols	defines the number of columns on the Sugarscape.
	 * @param	rows	defines the number of rows on the Sugarscape.
	 */
	public void nextScape()
	{
		//textArea.setText("");

		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append("nextScape #" + getTimePeriod()	+ "\n------------\n");	}

		incrementTime();	//increment the generation count
		timeElapsed();		//reset time slice counter to skip sleep()

		deathList = new LinkedList();
		birthList = new LinkedList();
		destinationI = -1;
		destinationJ = -1;
		float initialPollutant=0, postPollutant=0;

		//replenish sugar
		if(GoLconst.INITIATE_SEASONS)
			growGoodsBySeason();
		else growGoods();
		
		//Initialize Scoreboard Variables
		setDistrChildAdultSenior(null, 0);
		setDistrSeason(null, 0);
		setTotAge(null, 0);
		setTotSugarSpice(null, 0);

		//Display Sugar-n-Spice Distribution**
		if( GoLconst.DEBUG || GoLconst.DEBUG_SUGAR_PRODUCTION )
		{	showSugarStats(0,0,cellCols,cellRows);	}
		if( GoLconst.DEBUG || GoLconst.DEBUG_SPICE_PRODUCTION )
		{	showSpiceStats(0,0,cellCols,cellRows);	}

		//Measure Pollution before gathering
		if( GoLconst.DEBUG_POLLUTION )
			initialPollutant = sumPollutant();

		int index=0, i, j;
		//Shuffle citizens randomly before processing
		Collections.shuffle( zenList );
		otherTime += timeElapsed();

		//**Process all citizens except those born in current timePeriod
		int zenCount = zenList.size();
		while( index < zenCount )
		{	citizen = (Citizen)zenList.get(index++);
			i = citizen.getCol();
			j = citizen.getRow();
		
			if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR || GoLconst.DEBUG_MATING
				|| GoLconst.DEBUG_BARTER || GoLconst.DEBUG_INHERITANCE)
			{	textArea.append(citizen.showStats(""));	}		

			//**if Mating process is not active, then citizens must be replaced by new citizen on death
			//////if( GoLconst.INITIATE_MATING && !citizen.isAlive() )	//citizen has reached lifespan
			if( !citizen.isAlive() )	//citizen has reached lifespan
			{	if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_DEATH)
				{	textArea.append(citizen.showStats("Died! Old Age "));	}

				if( GoLconst.INITIATE_INHERITANCE )
					disburseInheritance(citizen);

				citizen.setCauseOfDeath("Old Age");
				deathList.add(citizen);
				population--;
				deathByDotage++;
				setCellspaceStats(cell[i][j].citizen, DEATHEVENT);
				cell[i][j].citizen = null;	//citizen killed
				if(!GoLconst.INITIATE_MATING)
				{	
				    createCitizen(null, null, i, j);		//generate new citizen
					 											// to replace dead one
					if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_DEATH)
					{	textArea.append(citizen.showStats("Citizen replaced! "));	}

					if (GoLconst.DEBUG || GoLconst.DEBUG_CRITICAL_ERROR) 
					{	if (cell[i][j].citizen == null)
							textArea	.append("1007)ERROR!! Unable to replace citizen\n");
					}
				}	
				continue;
			}
			if( GoLconst.DEBUG || GoLconst.DEBUG_DISEASE || GoLconst.DEBUG_DISEASE_COST )
			{	textArea.append("\n" + citizen.getFamily() + "\n");
				textArea.append("Disease sugar cost = " + 
							GoLconst.DISEASE_COST_SUGAR * citizen.getDiseaseCount()+"\n");
				textArea.append("Disease spice cost = " + 
							GoLconst.DISEASE_COST_SPICE * citizen.getDiseaseCount()+"\n");
			}
			
			if( !( citizen.eatSugar() && citizen.eatSpice() ) )
			{	if( GoLconst.INITIATE_MATING && GoLconst.INITIATE_INHERITANCE )
					disburseInheritance(citizen);

				citizen.setCauseOfDeath("Starvation" + (citizen.getSugar() < 0 ? " sugar" : " spice") );
				if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_DEATH)
				{	textArea.append(citizen.showStats("Died-Starvation! "));	}

				deathByStarvation++;

				setCellspaceStats((Citizen)cell[i][j].citizen, DEATHEVENT);
				
				cell[i][j].citizen = null;	//citizen killed
				if(!GoLconst.INITIATE_MATING)
				{	
				    createCitizen(null, null, i, j);		//generate new citizen
					 											// to replace dead one
					if( GoLconst.DEBUG || GoLconst.DEBUG_CITIZEN_DEATH)
					{	textArea.append(citizen.showStats("Citizen replaced! "));	}

					if (GoLconst.DEBUG || GoLconst.DEBUG_CRITICAL_ERROR) 
					{	if (cell[i][j].citizen == null)
							textArea	.append("1007)ERROR!! Unable to replace citizen\n");
					}
				}
				else
				{
					deathList.add(citizen);
					population--;
				}
				continue;
			}

			/*Search & Gather Food*/
			if( !searchGridGoods(citizen) )			//if no resources available
			{	searchFarthestCell(citizen);	}		//goto farthest cell

			//Destination determined by one of the 2 searches above, -1 if search failed
			if( destinationI > -1 )
				moveNGather( i, j, destinationI, destinationJ);
			gatherTime += timeElapsed();

			
			//What the hell is this IF all about?
			if( GoLconst.INITIATE_MATING && ((GoLconst.MATING_FEMALE_MIN < GoLconst.MATING_MALE_MIN) ?
			GoLconst.MATING_MALE_MIN < getTimePeriod() : GoLconst.MATING_FEMALE_MIN < getTimePeriod()) )
			{
				// ** Go Forth & Multiply
				if( GoLconst.DEBUG || GoLconst.DEBUG_MATING	|| GoLconst.DEBUG_MATING_SEARCH )
				{	textArea.append( "\nInitiating mating processes for: (" + 
							citizen.getID() + ")" + citizen.getFamily() + "\n");
				}
	
				if( citizen.ofMatingAge() && citizen.hasSurplus()
								&& cellNMatesAvailable(citizen) )
				{	
					if( mateList.size() > 1 )
					{	shuffleMates();
						pickMate();
					}
					//if( mate != null )
					{	bearChild(citizen, mate, destinationI, destinationJ);
						mate.setRanking();
						mateList.clear();
						mate = null;
					}
				}
				else if( GoLconst.DEBUG || GoLconst.DEBUG_MATING	|| GoLconst.DEBUG_MATING_SEARCH )
				{	textArea.append( "\tUnsuccessful attempt to mate\n");	}
				
				citizen.setRanking();
				mateTime += timeElapsed();
			}

			// **Trade processes**
			if( GoLconst.INITIATE_BARTER )
			{	if( GoLconst.DEBUG || GoLconst.DEBUG_BARTER || 
					GoLconst.DEBUG_BARTER_SORT	|| GoLconst.DEBUG_BARTER_EXCHANGE )
				{	textArea.append( "\nInitiating barter processes for: (" + 
							citizen.getID() + ")" + citizen.getFamily() + "\n");
				}
				searchNBarter(citizen);
				barterTime += timeElapsed();
			}
				
			// **Culture processes**
			if( GoLconst.INITIATE_CULTURE )
			{	if( GoLconst.DEBUG || GoLconst.DEBUG_CULTURE || GoLconst.DEBUG_CULTURE_TRANSMIT
					|| GoLconst.DEBUG_CULTURE_VOLATILE )	//|| GoLconst.DEBUG_CULTURE_STICKY 
				{	textArea.append( "\nInitiating culture processes for: (" + 
							citizen.getID() + ")" + citizen.getFamily() + "\n");
				}
				searchNFlip(citizen);
				cultureTime += timeElapsed();
			}
			
			// **Disease check & treatment**
			if( GoLconst.INITIATE_DISEASE )
			{
				if(GoLconst.DEBUG || GoLconst.DEBUG_DISEASE || GoLconst.DEBUG_DISEASE_TREAT
								|| GoLconst.DEBUG_DISEASE_TRANSMIT)
				{	textArea.append("\nInitiating medical checkup & treatment for: (" + 
							citizen.getID() + ")" + citizen.getFamily()+"\n");							
				}
				if(citizen.getDiseaseCount() == 0)
				{	if(GoLconst.DEBUG || GoLconst.DEBUG_DISEASE || GoLconst.DEBUG_DISEASE_TREAT)
						textArea.append(citizen.getFamily() + " has no diseases\n");
				}
				else
				{
					if(GoLconst.DEBUG || GoLconst.DEBUG_DISEASE || GoLconst.DEBUG_DISEASE_TREAT)
						textArea.append("Disease count before treatment = " + 
									citizen.getDiseaseCount() + "\n");
					checkNTreat(citizen);
				}
			}
				
			//Initialize Scoreboard Variables
			setDistrChildAdultSenior(citizen, 1);
			setDistrSeason(citizen, 1);
			setTotAge(citizen, 1);
			setTotSugarSpice(citizen, 1);
			otherTime += timeElapsed();
		} //end while( index < zenCount )
		setDistrGroups();
//		for(int gCount=0; gCount<groupCount; gCount++)
//		{	textArea.append(rangeName[gCount] + " = " + rangeTally[gCount] + "\n");	}
		cultureTime += timeElapsed();


		if( GoLconst.DEBUG_CITIZEN_DEATH )
		{	textArea.append("\tDeaths = " + deathList.size() + "\n");	}
		if( GoLconst.DEBUG_CITIZEN_BIRTH )
		{	textArea.append("\tBirths = " + birthList.size() + "\n");	}

		showPopulationStats(2);
		//Remove dead citizens from citizen list
		zenList.removeAll(deathList);
		//Add newborns to citizen list
		zenList.addAll(birthList);

		//Empty dead citizens list
		deathList.removeAll(deathList);

		//Empty newborn list
		birthList.removeAll(birthList);

		//Disperse Pollution
		if( GoLconst.INITIATE_POLLUTION )
			dispersePollution();

		//Measure Pollution after all processes
		if( GoLconst.DEBUG_POLLUTION )
		{	postPollutant = sumPollutant();
			textArea.append("Pollutant before year " + getTimePeriod() + ": "
				+ GoLconst.customFormat("###.##", initialPollutant) + ", at year end: "
				+ GoLconst.customFormat("###.##", postPollutant) + " Change = "
				+ GoLconst.customFormat("###.##", initialPollutant-postPollutant) + "\n" );
		}
	
		otherTime += timeElapsed();
	}

	/**
	 * Search process for citizens seeking sugar and spice on the 
	 * Sugarscape. Search area is defined by the vision of each citizen.
	 * The most fertile or a random selection among equally fertile 
	 * locations is made.
	 * @param citizen	the citizen for whom the method is being executed.
	 * @return	boolean, signalling the success/failure of the search.
	 */
	private  boolean searchGridGoods( Citizen citizen )
	{	int i0, j0, ix, jx;		//variables defining citizen's search grid
		destinationI = -1;
		destinationJ = -1;
		int currCitizenVision, col, row;

		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tsearchGridSugar(" + citizen.getFamily() + ")\n" );	}

		/*determine current citizen's vision grid*/
		currCitizenVision = citizen.getVision();
		col = citizen.getCol();
		row = citizen.getRow();
		i0 = col - currCitizenVision;
		j0 = row - currCitizenVision;
		ix = col + currCitizenVision;
		jx = row + currCitizenVision;

		//Ensure Vision Grid Does Not Extend Beyond Cellspace
		if(i0 < 0)	{i0 = 0;}
		if(j0 < 0)	{j0 = 0;}
		if(ix >= cellCols)
		{	ix = cellCols - 1;	}
		if(jx >= cellRows)
		{	jx = cellRows - 1;	}

		// Start mem vars with 'm'**
		int mSugarMorsels = (int)citizen.getSugar() / citizen.getMetabSugar();
		int mSpiceMorsels = (int)citizen.getSpice() / citizen.getMetabSpice();
		int mNeed;		//what good is dear - sugar or spice?
		if( mSugarMorsels < mSpiceMorsels )
			mNeed = -1;		//get sugar
		else if( mSugarMorsels > mSpiceMorsels )
			mNeed = 1;		//get spice
		else mNeed = 0;		//get both

		// Seek best supply of required goods**
		float currCellSugar = 0f;
		float currCellSpice = 0f;
		float currValueSugar, currValueSpice, currCellPollution;
		float destValueSugar = 0f, destValueSpice = 0f;
		int destinations = 0;

		if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR )
		{	textArea.append("\nVision = " + citizen.getVision() + "\n" );
			textArea.append("Search grid for curr citizen ->i0 = " + i0 + "  j0 = "
							+ j0 + "\tix = " + ix + "  jx = " + jx + "\n" );
		}
		//Find highest qty of required goods in any one cell
		for(int i=i0; i<=ix; i++)
		{	for(int j=j0; j<=jx; j++)
			{	currCellSugar = cell[i][j].getSugar();
				currCellSugar = currCellSugar > 0 ? currCellSugar : 0;  
				currCellSpice = cell[i][j].getSpice();
				currCellSpice = currCellSpice > 0 ? currCellSpice : 0;
				currCellPollution = cell[i][j].getPollution();

				if( (currCellSugar > 0 || currCellSpice > 0)
						&& cell[i][j].citizen == null )
				{	currValueSugar = ((float)( currCellSugar /
										(1f + currCellPollution) ) );
					currValueSpice = ((float)( currCellSpice /
										(1f + currCellPollution) ) );
				}
				else	//no sugar or spice available or cell occupied
				{	continue;	}

				if( (mNeed == -1 && (currValueSugar > destValueSugar ||
					(currValueSugar == destValueSugar && currValueSpice > destValueSpice)) )
					||
					(mNeed == 1 && (currValueSpice > destValueSpice ||
					(currValueSpice == destValueSpice && currValueSugar > destValueSugar)) )
					||
					(mNeed == 0 && ( (currValueSugar+currValueSpice) >
										(destValueSugar+destValueSpice) ) ) )
				{
					destValueSugar = currValueSugar;
					destValueSpice = currValueSpice;
					destinations = 1;
					destinationI = i;
					destinationJ = j;
					if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR )
					{	textArea.append("Better Dest." + cell[i][j].showStats(i,j) );	}
				}
				//Another Destination
				else if(currValueSugar==destValueSugar && currValueSpice==destValueSpice)
				{	destinations++;
					if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR )
					{	textArea.append("Cell["+i+"]["+j+"] contains "
										+ currCellSugar + " sugar & "
										+ currCellSpice + " spice & "
										+ currCellPollution + " pollution\n");
					}
				}
			}
		}
		if( destinations == 0 )		//if no goods found
		{	
			if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR )
			{	textArea.append("No goods found around [" + col + "][" + row + "]\n" );	}
			return false; 
		} 

		if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR )
		{	textArea.append("Ideal Cell: sugar = " 
				+ GoLconst.customFormat("###.##", destValueSugar) 
				+ "\tspice = " + GoLconst.customFormat("###.##", destValueSpice) 
				+ "\tDestinations = " + destinations + "\n" );
		}

		boolean foundDestination = false;
		int randomDest = 0;
		if( destinations == 1 )
			foundDestination = true;
		else	// **More than onepossible destination**
		{
			randomDest = (int)(Math.random()* destinations) + 1;
			if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR )
			{	textArea.append("Random Destination = " + randomDest + "\n" );	}

		}

		for(int i=i0; i<=ix && !foundDestination; i++)
		{	for(int j=j0; j<=jx; j++)
			{
				if( cell[i][j].citizen != null )
					continue;

				currCellSugar = cell[i][j].getSugar();
				currCellSugar = currCellSugar > 0 ? currCellSugar : 0;  
				currCellSpice = cell[i][j].getSpice();
				currCellSpice = currCellSpice > 0 ? currCellSpice : 0;
				currCellPollution = cell[i][j].getPollution();

				currValueSugar = ((float)( currCellSugar /
									(1f + currCellPollution) ) );
				currValueSpice = ((float)( currCellSpice /
									(1f + currCellPollution) ) );

				if( currValueSugar==destValueSugar && currValueSpice==destValueSpice )
				{	if( randomDest == 1)
					{	foundDestination = true;	//get out of the 2 For loops
						destinationI = i;
						destinationJ = j;
						break;
					} else	//Skip This Destination
					{	randomDest--;	}
				}
			}
		}

		if(destinationI < 0)		//all cells occupied OR
		{	return false;	} 			//no sugar available within area of vision
		else
		{

			if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR )
			{	textArea.append("+--Search over, RandomDest = " + randomDest
					+ "\tHighest sugar-n-spice in cell" 
					+ cell[destinationI][destinationJ].showStats
													(destinationI, destinationJ));
			}
			return true;
		}
	}

	/**
	 * Search process for farthest cell within range specified by 
	 * citizen's vision.
	 * The most distant or a random selection from equally distant 
	 * locations is made.
	 * @param citizen	the citizen for whom the method is being executed.
	 * @return	boolean, signalling the success/failure of the search.
	 */
	private  boolean searchFarthestCell( Citizen citizen)
	{	int i0, j0, ix, jx;		//variables denoting diagonal corners of grid
		destinationI = -1;
		destinationJ = -1;
		int currCitizenVision, col, row;

		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tsearchFarthestCell("+citizen.getFamily()+")\n" );	}
		if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR )
		{	Toolkit.getDefaultToolkit().beep();
			textArea.append("Picking farthest cell to relocate!!\n");	}
		
		/*determine current citizen's vision grid*/
		currCitizenVision = citizen.getVision();
		col = citizen.getCol();
		row = citizen.getRow();
		i0 = col - currCitizenVision;
		j0 = row - currCitizenVision;
		ix = col + currCitizenVision;
		jx = row + currCitizenVision;
		/*ensure vision grid does not extend beyond Cellspace*/
		if(i0 < 0)	{i0 = 0;}
		if(j0 < 0)	{j0 = 0;}
		if(ix >= cellCols)
		{	ix = cellCols - 1;	}
		if(jx >= cellRows)
		{	jx = cellRows - 1;	}

		//**Find Cell(s) at Greatest Distance - Randomly Select One Destination**
		int[][] distance = new int[cellCols][cellRows];
		int distanceVal = 0;
		int destinations = 0;

		/*Initialize Distance Array*/
		for(int i=0; i<cellCols; i++)
			for(int j=0; j<cellRows; j++)
				distance[i][j] = 0;

		/*Calculate Distance of Each Cell Within Scope of Vision*/
		for(int i=i0; i<=ix; i++)
			for(int j=j0; j<=jx; j++)
			{	if(!cells[i][j] && cell[i][j].citizen == null  )
				{	//calculate distance to this unoccupied cell
					distance[i][j] = Math.abs(col-i) + Math.abs(row-j);
					//if this cell is more distant than current greatest distance
					if(distance[i][j] > distanceVal)
					{	distanceVal  = distance[i][j];
						//Initialize with discovery of greater distance
						destinations = 1;
						if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR)
						{	textArea.append("New Destination = " + destinations
								+ "\t Distance  of cell["+i+"]["+j+"] = "
								+ distance[i][j] + "\n" );
						}
					}
					// another destination found, equal to current greatest distance
					else if( distance[i][j] == distanceVal  && distanceVal > 0)
					{	destinations++;		}
				}
				else
					distance[i][j] = 0;	//cell occupied, distance irrelevant
			}

		if( distanceVal == 0 )
		{	if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR)
			{	textArea.append("NO food, NO empty cells, staying put at "
							+ "[" +col+"]["+row+"]\n" );
			}
			return false;	//No available cell within scope of vision
		}
		/*Randomly set one of most distant cells as destination*/
		int randomDest = (int)(Math.random()* destinations) + 1;
		boolean foundDestination = false;

		if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR )
		{	textArea.append("Random Destination = " + randomDest + "\n" );	}

		for(int i=i0; i<=ix; i++)
		{	if( foundDestination )	{	break;	}

			for(int j=j0; j<=jx; j++)
			{	if( distance[i][j] == distanceVal )
				{	if( randomDest == 1)
					{	foundDestination = true;	//get out of the 2 For loops
						destinationI = i;
						destinationJ = j;
						if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR)
						{	textArea.append("***Destination found!!"
								+ "\t Distance to cell["+i+"]["+j+"] = "
								+ distance[i][j] + "\n" );
						}
						break;
					}
					else
					{	randomDest--;	}
				}
			}
		}
		if(destinationI < 0)		//all cells occupied OR
			return false;			//no sugar available within area of vision
		else return true;
	}

	/**
	 * Moves citizen to a new location previously identified by the 
	 * search methods. If sugar and/or spice available at new location, 
	 * it is gathered by citizen. 
	 * @param fromI	column value of previous location.
	 * @param fromJ	row value of previous location.
	 * @param toI	column value of new location.
	 * @param toJ	row value of new location.
	 * @return	boolean, signalling successful relocation.
	 */
	private  boolean moveNGather(int fromI, int fromJ, int toI, int toJ)
	{
		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tmoveNGather("+fromI+","+fromJ
					+","+toI+","+toJ+")\n" );
		}

		if( cell[toI][toJ].citizen != null )
		{	if( GoLconst.DEBUG || GoLconst.DEBUG_CRITICAL_ERROR )
			{	textArea.append(msgStr + "\nTime period: " + getTimePeriod());
				citizen.showStats("Citizen");
				cell[toI][toJ].citizen.showStats("Destination occupant");
			}
			return false;
		}

		// ***Move Citizen to new location**
		cell[toI][toJ].citizen = cell[fromI][fromJ].citizen;
		cell[fromI][fromJ].citizen = null;
		cell[toI][toJ].citizen.setCol(toI);
		cell[toI][toJ].citizen.setRow(toJ);
		if( cell[toI][toJ].getSugar() > 0 )
		{	cell[toI][toJ].citizen.setSugar( cell[toI][toJ].citizen.getSugar()
										+ cell[toI][toJ].getSugar() );
										
			cell[toI][toJ].setSugar(0f); //Sugar consumed, initialize to zero
		}
		if( cell[toI][toJ].getSpice() > 0 )
		{	cell[toI][toJ].citizen.setSpice( cell[toI][toJ].citizen.getSpice()
										+ cell[toI][toJ].getSpice() );
			cell[toI][toJ].setSpice(0f); //Spice consumed, initialize to zero
		}

		cells[fromI][fromJ] = false;	//citizen has moved to new cell
		cells[toI][toJ] 	= true;		//new location for citizen

		if( GoLconst.DEBUG || GoLconst.DEBUG_SEARCH_SUGAR)
		{	textArea.append("moveNGather: Completed move from "
				+ "[" +fromI+"]["+fromJ+"]" + " to " + "[" +toI+"]["+toJ+"]\n" );
		}
		return true;
	}

	/**
	 * Examines adjacent cells to determine if any prospective mates are
	 * available and for an empty cell in which to bear a child.
	 * Prospective mates are checked for compatibility and child-supporting
	 * capability. If all Ok, they are added to a prospective mates list.
	 * 
	 * @param	citizen	citizen for whom mates are being sought.
	 * @return	boolean, signalling availability of atleast one mate and 
	 * empty cell for newborn child.
	 */
	public  boolean cellNMatesAvailable(Citizen citizen)
	{
		int i0, ix, j0, jx;
		destinationI = -1; 	destinationJ = -1;	//variables reused in searchSugar process
		int freeCellCntr = 0;
		//int index;

		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\t\tcellNMatesAvailable(" + citizen.getFamily() + ")\n" );
		}
		if( GoLconst.DEBUG || GoLconst.DEBUG_MATING	|| GoLconst.DEBUG_MATING_SEARCH )
		{	textArea.append("\t" + citizen.getFamily() + 
				" is of mating age & has sufficient surplus\n\tSeeking empty cell & mates\n" );
		}

		int col = citizen.getCol();
		int row = citizen.getRow();

		//Define Search Grid For Soulmate
		i0 = col - 1;	ix = col + 1;
		j0 = row - 1;	jx = row + 1;
		//Ensure Search does not extend beyond grid borders
		if(i0 < 0)	{ i0 = 0; }
		if(j0 < 0)	{ j0 = 0; }
		if(ix >= cellCols) { ix = cellCols - 1; }
		if(jx >= cellRows) { jx = cellRows - 1; }

		//Find Empty Cells
		for(int i=i0; i<=ix; i++)
			for(int j=j0; j<=jx; j++)
			{	if( cell[i][j].citizen == null )
				{
					destinationI = i;
					destinationJ = j;
					freeCellCntr++;
				}
				//Cell Occupied by a someone else
				else if( col != i && row != j )
				{
					mate = cell[i][j].citizen;
					if( citizen.ofCompatibleSex(mate) && mate.ofMatingAge()
						&& mate.pastMatingInterval() && mate.hasSurplus()
						&& (GoLconst.MATING_KIN_ALLOW ||!citizen.isKinOf(mate) ) )
					{	if( GoLconst.DEBUG || GoLconst.DEBUG_MATING || GoLconst.DEBUG_MATING_SEARCH )
						{	textArea.append(mate.showStats("\tFound mate : ") );	}
						mateList.add(mate);
					}
				}
			}
		//Generate Random Number & Select Corresponding Cell
		boolean foundDestination = false;
		if( freeCellCntr > 1 )		//One or More Adjacent Cells Empty
		{	int randomDest = (int)(Math.random() * freeCellCntr);
			for(int i=i0; i<=ix && !foundDestination; i++)
				for(int j=j0; j<=jx; j++)
				{	if( randomDest == 0 && cell[i][j].citizen == null ) //Select Current Location
					{	destinationI = i;	destinationJ = j;
						foundDestination = true;
						if( GoLconst.DEBUG || GoLconst.DEBUG_MATING || GoLconst.DEBUG_MATING_SEARCH )
						{	if( !mateList.isEmpty() )	
							textArea.append( "\tNew child's destination: ["+destinationI+"]["+destinationJ+"]\n" );	
						}
						break;
					} else if( cell[i][j].citizen == null )
						randomDest--;		//Skip This Location
				}
		}
		else if( freeCellCntr == 1 )
		{	foundDestination = true;	}

		if( !foundDestination || mateList.isEmpty() )
		{	if( GoLconst.DEBUG || GoLconst.DEBUG_MATING || GoLconst.DEBUG_MATING_SEARCH )
			{	textArea.append( "\tfoundDestination="+(foundDestination?"TRUE":"FALSE")+"\t"
					+ (mateList.isEmpty() ? "Mate list empty!!\n" : ("Mates = "+mateList.size())
					+"New child's destination: ["+destinationI+"]["+destinationJ+"]\n") );
				//textArea.append( "\tUnsuccessful attempt to mate\n");
			}
			return false;
		}
		return true;
	}

	/**
	 * Shuffles list of mates before selection of final partner.
	 * 
	 */
	public  void shuffleMates()
	{
		int mateCount = mateList.size();
		int index;
		java.util.List tmpList = new LinkedList();

		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tshuffleMates() - " + mateCount + "\n" );	}

		while( mateCount > 0 )
		{	index = (int)(Math.random() * mateCount);
			tmpList.add( mateList.get(index) );
			mateList.remove(index);
			mateCount--;
		}

		ListIterator tmpIter = tmpList.listIterator();
		index = 0;
		while( tmpIter.hasNext() )
		{	tmpIter.next();
			mateList.add( tmpList.get(index++) );
		}
		tmpIter = null;
		tmpList = null;
	}

	/**
	 * Attaches weights to each mate based on their ranking. Higher 
	 * ranking increases selection chances.<BR>
	 * Randomly selects mate. 
	 */
	public  void pickMate()
	{
		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tpickMate()\n" );	}

		int index = 0;
		int totalRanking = 0;
		int pickup;

		ListIterator mateIter = mateList.listIterator();
		while( mateIter.hasNext() )
		{	mateIter.next();
			mate = (Citizen)mateList.get(index++);
			totalRanking += mate.getRanking();
		}
		pickup = (int)Math.random() * totalRanking;
		index = 0;
		mateIter = mateList.listIterator();
		while( mateIter.hasNext() && pickup > 0 )
		{	mateIter.next();
			mate = (Citizen)mateList.get(index++);	//contains chosen mate
			pickup -= mate.getRanking();			//when loop terminates
		}
		if( GoLconst.DEBUG || GoLconst.DEBUG_MATING || GoLconst.DEBUG_MATING_SELECTION )
		{	textArea.append( mate.showStats("\tChosen Mate: ") );	}
	}

	/**
	 * Bears child inheriting characteristics of citizen and mate. 
	 * Transfers wealth from parents to child as its inheritance. 
	 * Updates mating details for parents.
	 * 
	 * @param	citizen	active parent, can be male or female.
	 * @param	mate	selected partner, opposite sex of citizen.
	 * @param	col	column location of newborn citizen.
	 * @param	row	row location of newborn citizen.
	 */
	public  void bearChild(Citizen citizen, Citizen mate, int col, int row)
	{	Citizen child;
		Cell currCell = cell[col][row];
		
		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tbearChild(" + citizen.getFamily()+ "," + mate.getFamily()
												+ "," + col + "," + row + ")\n" );
		}
		cells[col][row] = true;

		//Create New Child of citizen & mate
		currCell.citizen = new Citizen(citizen, mate, col, row);
		child = cell[col][row].citizen;
		
		//Grab Sugar in Birth Cell
		if( currCell.getSugar() > 0 )
		{	child.setSugar( child.getSugar() + currCell.getSugar() );
			currCell.setSugar(0f);	// Sugar at New Location Consumed
		}
		if( currCell.getSpice() > 0 )
		{	child.setSpice( child.getSpice() + currCell.getSpice() );
			currCell.setSpice(0f);	//Child Consumes Spice at New Location
		}

		try {	birthList.add(child);}
		catch (Exception e){	System.out.println("Exception(birth of child): ");	}
		population++;
		setCellspaceStats(cell[col][row].citizen, BIRTHEVENT);
		if( GoLconst.DEBUG || GoLconst.DEBUG_MATING || GoLconst.DEBUG_MATING_BIRTH
				|| GoLconst.DEBUG_CITIZEN_BIRTH)
		{	textArea.append( child.showStats("\tNew child: ") );	}

		//Change Parent Properties to Reflect Child Birth
		citizen.setLastMating(getTimePeriod());
		citizen.addChild( child );
		mate.setLastMating(getTimePeriod());
		mate.addChild( child );
	}

	/**
	 * Disburses wealth of a dead citizen equally among its offspring.
	 * Executed only when inheritance process is activated.
	 * 
	 * @param	citizen	deceased citizen. 
	 */
	public  void  disburseInheritance( Citizen citizen )
	{
		int cntr = 0, parentSugar, parentSpice;
		Citizen child;
		
		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tdisburseInheritance for (" + citizen.getFamily()+ ")\n" );
		}
		
		int childCount = citizen.checkChildren();
		parentSugar = (int)citizen.getSugar();
		parentSpice = (int)citizen.getSpice();

		//check if sufficient sugar available to give children
		if( (parentSugar >= childCount || parentSpice >= childCount)
				&& childCount > 0)
		{	while( cntr < childCount )
			{	child = (Citizen) citizen.childList.get( cntr );

				if( GoLconst.DEBUG_INHERITANCE )
					child.showStats("\n\tchild" + (cntr+1) + ": ");	

				child.setSugar( child.getSugar() + (parentSugar/childCount) );
				child.setSpice( child.getSpice() + (parentSpice/childCount) );

				if( GoLconst.DEBUG_INHERITANCE )
					textArea.append( "\tAfter Inheritance, child's sugar = "
						+ child.getSugar() + "\tspice = " + child.getSpice() + "\n");
				cntr++;
			}
		}
		else
		{	if( GoLconst.DEBUG_INHERITANCE )
				textArea.append("Not enough sugar or no children!\n");
		}
	}
	
	/**
	 * Barter process defining needs evaluation, prospects search, negotiation and 
	 * mutually beneficial exchange of goods by denizens of the Sugarscape. 
	 * This method calculates the Marginal Rate of Substitution for a citizen and builds
	 * a list of compatible trade partners.
	 * It then ranks those offers in order of desirability to the citizen.
	 * Finally, it completes the exchange of goods with one or more traders based on 
	 * how much the citizen wishes to trade.
	 * 
	 * @param	citizen	citizen currently exploring trade oppurtunities.
	 */
	private void searchNBarter( Citizen citizen )
	{
		traderList = new LinkedList();
		int surplusA, surplusB;	//spUnitsA, suUnitsA, spUnitsB, suUnitsB, TUDA, TUDB;
		float MRSA, MRSB;
		int i0, ix, j0, jx;

		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tsearchNBarter for (" + citizen.getFamily()+ ")\n" );
		}

		int col = citizen.getCol();
		int row = citizen.getRow();
				
		citizen.setSpUnits();
		citizen.setSuUnits();
		citizen.setTUD();
		MRSA 	 = citizen.setMRS(); 
		surplusA = citizen.setSurplus();
		
		//Cautious Traders Only Trade If They Have a Surplus
		if( citizen.isCautious() && surplusA == 0 )
		{	if( GoLconst.DEBUG_BARTER || GoLconst.DEBUG)
				textArea.append( "\tNo surplus for barter\n" );
			return;
		}
		//Define Search Grid For citizen
		i0 = col - 1;	ix = col + 1;
		j0 = row - 1;	jx = row + 1;
		if( i0 < 0 )	{ i0 = 0;	}
		if( j0 < 0 )	{ j0 = 0;	}
		if( ix>= cellCols )	{ ix = cellCols - 1; }
		if( jx>= cellRows )	{ jx = cellRows - 1; }
		
		//Select Suitable Trading Partners
		for( int i = i0; i <= ix; i++ )
		{	for( int j = j0; j <= jx; j++ )
			{	if( cell[i][j].citizen == null )
					continue;
					
				trader = cell[i][j].citizen;
				MRSB = trader.getMRS();
				surplusB = trader.getSurplus();
				
				//Cautious Traders Only Trade If They Have a Surplus
				if( trader.isCautious() && surplusB == 0 )
					continue;	//trader does not have any surplus to trade
				if( ( citizen.isCautious() && trader.isCautious() ) &&
						Citizen.preferSame(MRSA, MRSB) ) // both prefer the same good
					continue;
				if( MRSA == MRSB )
					continue;	//neither party benefits from trade
				if( GoLconst.DEBUG || GoLconst.DEBUG_BARTER  )
					textArea.append(trader.showStats("\tFound trader: "));
				traderList.add(trader);
			}
		}
		int nCount = traderList.size();
		if( nCount == 0 )
		{
			if( GoLconst.DEBUG || GoLconst.DEBUG_BARTER  )
				textArea.append("\tNo prospects available for trade\n");
			return;	//No partners to Barter with
		}
		if( traderList.size() > 1 )
			sortTraders( citizen );	//Rank Traders By Value to citizen
			
		//Barter Until No More surplus or trade partners 
		int index = 0;
		while( index < nCount && surplusA > 0 )
		{	trader = (Citizen)traderList.get(index++);
			surplusA = tradeGoods( citizen, trader );
		}
	}
	
	/**
	 * Method called when multiple prospects available for trade with 
	 * a citizen. Offers are sorted by price variance.
	 * @param citizen	citizen currently exploring trade oppurtunities.
	 */
	private void sortTraders( Citizen citizen )
	{	java.util.List tmpList;
		tmpList = new LinkedList();
		Citizen zenA = null, zenB = null;
		float MRSgapA, MRSgapB;
		int index; 
		boolean sorted = false;
		
		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tsortTraders (" + citizen.getFamily()+ ")\n" );
		}

		if( GoLconst.DEBUG_BARTER_SORT || GoLconst.DEBUG_BARTER || GoLconst.DEBUG )
			textArea.append("\tMultiple partners found, sorting by MRS variance...\n");
		
		while( !sorted )
		{	index = 0;
			zenA = (Citizen)traderList.get(index++);
			sorted = true;
			while( index < traderList.size() )
			{	zenB = (Citizen)traderList.get(index++);
				//Get & Set MRS Return the Same Value Except Set recalculates
				MRSgapA = Math.abs( citizen.getMRS() - zenA.getMRS() );
				MRSgapB = Math.abs( citizen.getMRS() - zenB.getMRS() );
				
				if( MRSgapA < MRSgapB )
				{	tmpList.add(zenB);
					sorted = false;
				} else
				{	tmpList.add(zenA);
					zenA = zenB;
				}
			}
			tmpList.add(zenA);
			
			if( GoLconst.DEBUG_BARTER_SORT || GoLconst.DEBUG_BARTER || GoLconst.DEBUG )
			{	Citizen trader;
				for(index = 0; index < traderList.size(); index++)
				{	trader = (Citizen)traderList.get(index);
					trader.showStats("\tTrader "+(index+1)+")");
				}
			}			
			traderList.removeAll(traderList);
			traderList.addAll(tmpList);
			tmpList.removeAll(tmpList);
		}
	}
	
	/**
	 * Determines the trade price, flow of goods and units to be exchanged. 
	 * It then executes the exchange and recalculates preferences of the
	 * citizen and his/her trading partner.
	 * @param citizen	individual seeking to trade
	 * @param trader	individual whose offer has been accepted
	 * @return	amount of surplus still available for trade by citizen.
	 */
	private int tradeGoods(Citizen citizen, Citizen trader)
	{	float barterPrice;
		int surplus = 1;
		float qtyReturn = 0f;
		
		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\ttradeGoods (" + citizen.getFamily() 
						+"," + trader.getFamily() + ")\n" );
		}

		citizen.setSpUnits();
		citizen.setSuUnits();
		citizen.setTUD();
		trader.setSpUnits();
		trader.setSuUnits();
		trader.setTUD();
		float MRSA = citizen.setMRS(); 
		float MRSB = trader.setMRS();
		int surplusA = citizen.setSurplus();
		int surplusB = trader.setSurplus();		

		if( GoLconst.DEBUG_BARTER_EXCHANGE || GoLconst.DEBUG_BARTER 
											|| GoLconst.DEBUG )
		{	textArea.append( "\tBefore Exchange:\n" );
			textArea.append(citizen.showStats( "\tCitizen") );
			textArea.append(trader.showStats( "\tTrader ") );
		}
		
		//If Either Party is Cautious, only Available Surplus is Traded
		if( citizen.isCautious() && trader.isCautious() )
			surplus	   = surplusA <= surplusB ? surplusA/2 : surplusB/2;
		//Determine Barter price 
		barterPrice 	= (float)Math.sqrt( MRSA * MRSB );

		if( GoLconst.DEBUG_BARTER_EXCHANGE || GoLconst.DEBUG_BARTER || GoLconst.DEBUG )
			textArea.append("\tFinal tradeable surplus = " + surplus + 
							" @ Price " + barterPrice + "\n");
		
		//**Proceed with Exchange
		//Citizen gets sugar, Trader gets spice
		if( MRSA > MRSB || citizen.needsSugar() || trader.needsSpice())	
		{	
			if( barterPrice >= 1 )
			{	qtyReturn = surplus*barterPrice;
				trader.setSpice ( trader.getSpice()  + qtyReturn );
				citizen.setSugar( citizen.getSugar() + surplus );
				citizen.setSpice( citizen.getSpice() - qtyReturn );
				trader.setSugar ( trader.getSugar()  - surplus );
				if( GoLconst.DEBUG_BARTER_EXCHANGE || GoLconst.DEBUG_BARTER || GoLconst.DEBUG )
				{	textArea.append("\t1)Citizen gets " + surplus + " sugar, Trader gets " +
									qtyReturn + " spice\n");
				}
			} else if( barterPrice > 0 )
			{	qtyReturn = surplus*(1/barterPrice);
				trader.setSpice ( trader.getSpice()  + surplus );
				citizen.setSugar( citizen.getSugar() + qtyReturn );
				citizen.setSpice( citizen.getSpice() - surplus );
				trader.setSugar ( trader.getSugar()  - qtyReturn );
				if( GoLconst.DEBUG_BARTER_EXCHANGE || GoLconst.DEBUG_BARTER || GoLconst.DEBUG )
				{	textArea.append("\t2)Citizen gets " + qtyReturn + " sugar, Trader gets " +
									surplus + " spice\n");
				}
			}
		//Citizen gets spice, Trader gets sugar
		} else if( MRSA < MRSB  || citizen.needsSpice() || trader.needsSugar())
		{	if( barterPrice >= 1 )
			{	qtyReturn = surplus*barterPrice;
				citizen.setSpice( citizen.getSpice() + qtyReturn );
				trader.setSugar ( trader.getSugar()  + (surplus) );
				trader.setSpice ( trader.getSpice()  - qtyReturn );
				citizen.setSugar( citizen.getSugar() - (surplus) );
				if( GoLconst.DEBUG_BARTER_EXCHANGE || GoLconst.DEBUG_BARTER || GoLconst.DEBUG )
				{	textArea.append("\t3)Citizen gets " + qtyReturn + " spice, Trader gets " +
									surplus + " sugar\n");
				}
			} else if( barterPrice > 0 )
			{	qtyReturn = surplus*(1/barterPrice);
				citizen.setSpice( citizen.getSpice() + surplus );
				trader.setSugar ( trader.getSugar()  + qtyReturn );
				trader.setSpice ( trader.getSpice()  - surplus );
				citizen.setSugar( citizen.getSugar() - qtyReturn );
				if( GoLconst.DEBUG_BARTER_EXCHANGE || GoLconst.DEBUG_BARTER || GoLconst.DEBUG )
				{	textArea.append("\t4)Citizen gets " + surplus + " spice, Trader gets " +
									qtyReturn + " sugar\n");
				}
			}
		}
		//Recalculate Surplus For Citizen & Trader
		surplusA = citizen.setSurplus();

		//Status of Barter Participants After Trade
		if( GoLconst.DEBUG_BARTER_EXCHANGE || GoLconst.DEBUG_BARTER 
											|| GoLconst.DEBUG )
		{	textArea.append( "\tAfter Exchange:\n" );
			textArea.append(citizen.showStats("\tCitizen") );
			textArea.append(trader.showStats("\tTrader ") );
		}
		return surplusA;
	}

	/**
	 * Scans the neighborhood and flips tags as directed by the 
	 * GoLconst.CITIZEN_CULTURE_TRANSFER variable
	 * @param citizen	whose cultural attributes are being absorbed / propogated 
	 */
	private void searchNFlip(Citizen citizen)
	{
		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tsearchNFlip (" + citizen.getFamily() + ")\tTransfer = " 
								+ GoLconst.CITIZEN_CULTURE_TRANSFER + "\n" );	}

		//**Build neighbor list**
		java.util.List neighborList;
		neighborList = new LinkedList();
		int i0, ix, j0, jx;
		int col = citizen.getCol();
		int row = citizen.getRow();
		Citizen neighbor;

		//Define Search Grid For Soulmate
		i0 = col - 1;	ix = col + 1;
		j0 = row - 1;	jx = row + 1;
		//Ensure Search does not extend beyond grid borders
		if(i0 < 0)	{ i0 = 0; }
		if(j0 < 0)	{ j0 = 0; }
		if(ix >= cellCols) { ix = cellCols - 1; }
		if(jx >= cellRows) { jx = cellRows - 1; }

		
		//Find Neighbors
		for(int i=i0; i<=ix; i++)
			for(int j=j0; j<=jx; j++)
			{	if( cell[i][j].citizen != null && (col != i && row != j) )
					neighborList.add(cell[i][j].citizen);
			}
		int nCount = neighborList.size();
		//textArea.append(nCount + "neighbors, --------------------------------------\n");
		if( nCount == 0 )
		{
			if( GoLconst.DEBUG || GoLconst.DEBUG_CULTURE || GoLconst.DEBUG_CULTURE_TRANSMIT
					|| GoLconst.DEBUG_CULTURE_STICKY || GoLconst.DEBUG_CULTURE_VOLATILE ) 
			{	textArea.append( "\nNo neighbors, terminating culture processes\n");	}
			return;
		}

		if( GoLconst.DEBUG || GoLconst.DEBUG_CULTURE || GoLconst.DEBUG_CULTURE_TRANSMIT
				|| GoLconst.DEBUG_CULTURE_STICKY || GoLconst.DEBUG_CULTURE_VOLATILE ) 
		{	textArea.append(citizen.showStats("\tCitizen: "));	}

		//Match random volatile tag and flip if needed
		boolean neighborsTUDBetter = true;
		int index = 0;
		while( index < nCount )
		{	neighbor = (Citizen)neighborList.get(index++);

			if( GoLconst.DEBUG || GoLconst.DEBUG_CULTURE || GoLconst.DEBUG_CULTURE_TRANSMIT ||
				GoLconst.DEBUG_CULTURE_STICKY || GoLconst.DEBUG_CULTURE_VOLATILE )
			{	textArea.append(neighbor.showStats("\tNeighbor: "));	}

			if( neighbor.getTUD() <= citizen.getTUD() )
				neighborsTUDBetter = false;	//all neighbors must have MRS>citizen 
											//for sticky tag flip

			//Randomly select a volatile tag
			int divider = (GoLconst.CITIZEN_CULTURE_DIVIDE>0) ?
							(GoLconst.CITIZEN_CULTURE_DIVIDE-1) : 0;
			int tagToFlip = divider + (int)((float)Math.random()* 
					(GoLconst.CITIZEN_CULTURE_TAG - GoLconst.CITIZEN_CULTURE_DIVIDE));

			if( GoLconst.DEBUG || GoLconst.DEBUG_CULTURE || GoLconst.DEBUG_CULTURE_VOLATILE )
			{	textArea.append("\tFlipping tag " + tagToFlip);	}

					
			if( citizen.getPhenoMeme(tagToFlip) != neighbor.getPhenoMeme(tagToFlip) )
			{	if(GoLconst.CITIZEN_CULTURE_TRANSFER == 1)	//absorb
				{	citizen.setPhenoMeme(tagToFlip, neighbor.getPhenoMeme(tagToFlip));	}
				else if(GoLconst.CITIZEN_CULTURE_TRANSFER == 2)	//propogate
				{	neighbor.setPhenoMeme(tagToFlip, citizen.getPhenoMeme(tagToFlip));	}
				else if(GoLconst.CITIZEN_CULTURE_TRANSFER == 3)	//by ranking
				{	if(citizen.getRanking() > neighbor.getRanking())
						neighbor.setPhenoMeme(tagToFlip, citizen.getPhenoMeme(tagToFlip));
					else if(citizen.getRanking() < neighbor.getRanking())
						citizen.setPhenoMeme(tagToFlip, neighbor.getPhenoMeme(tagToFlip));
				}
			}
			if( GoLconst.DEBUG || GoLconst.DEBUG_CULTURE || GoLconst.DEBUG_CULTURE_VOLATILE )
			{	textArea.append(citizen.showStats("\tCitizen: "));
				textArea.append("\n");	
			}
		}
		//Chk if sticky tags flip conditions exist & flip if required
		if( nCount >= GoLconst.CITIZEN_CULTURE_OVERWHELM && neighborsTUDBetter && 
					GoLconst.CITIZEN_CULTURE_DIVIDE > 0)
		{	int tagEnd = GoLconst.CITIZEN_CULTURE_DIVIDE;
			short tagCount[] = new short[tagEnd];
			boolean tagValue[] = new boolean[tagEnd];

			index = 0;
			neighbor = (Citizen)neighborList.get(index++);
			for(int i=0; i<tagEnd; i++)
			{	tagCount[i] = 1;
				tagValue[i] = neighbor.getPhenoMeme(i);
			}
			
			while( index < nCount )
			{	neighbor = (Citizen)neighborList.get(index++);
				for(int i=0; i<tagEnd; i++)
				{	if(neighbor.getPhenoMeme(i) == tagValue[i] )
						tagCount[i]++; 
				}
			}
			
			int tagToFlip = (int)( (float)Math.random() * GoLconst.CITIZEN_CULTURE_DIVIDE );
			if( tagCount[tagToFlip] == nCount )	//all neighbors have matching tag
			{	citizen.setPhenoMeme(tagToFlip, tagValue[tagToFlip]); 
				if( GoLconst.DEBUG || GoLconst.DEBUG_CULTURE || GoLconst.DEBUG_CULTURE_STICKY )
				{	textArea.append(citizen.showStats("\t==>Changing Sticky Tag!!\nCitizen: "));
					textArea.append("\n");
					Toolkit.getDefaultToolkit().beep();	
				}
			}
		}
		neighborList = null;
	}

	/**
	 * Method to format float and double vars for display on screen.
	 * 
	 * @param	pattern	format pattern.
	 * @param	value	float/double value to format.
	 * @return	string containing formatted value.
	 */

	/**
	 * Implements the disease processes for a citizen
	 * @param citizen	whose cultural attributes are being absorbed / propogated 
	 */
	private void checkNTreat(Citizen citizen)
	{
		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tcheckNTreat (" + citizen.getFamily() + "\n" );	}


				
		int dIndex=0, dCount = citizen.getDiseaseCount(),
			immuneIndex = -1;
		String immuneSys = citizen.getPImmuneStr(), 
				disease = "";

		while(dIndex < dCount)
		{
			disease = citizen.getDisease(dIndex);

			if(GoLconst.DEBUG || GoLconst.DEBUG_DISEASE || GoLconst.DEBUG_DISEASE_TREAT)
			{	textArea.append("Immune Sys before treatment = " + citizen.getPImmuneStr()+"\n"); 
				textArea.append("Disease to be treated = " + disease+"\n");
			}
			
			immuneIndex = immuneSys.indexOf(disease);
			if(immuneIndex < 0)
			{
				immuneIndex = citizen.getBestHammingMatch(disease);
				
				if(GoLconst.DEBUG || GoLconst.DEBUG_DISEASE || GoLconst.DEBUG_DISEASE_TREAT)
					textArea.append("Location of best match substring = " + immuneIndex);

				citizen.generateImmunity(disease, immuneIndex);
				break;
			}
			dIndex++;
		}
		//after immune sys tag flip, chk disease list to eliminate cured diseases
		dIndex=0;
		immuneSys = citizen.getPImmuneStr();
				
		if(GoLconst.DEBUG || GoLconst.DEBUG_DISEASE || GoLconst.DEBUG_DISEASE_TREAT)
		{	if(dCount > 0)
			{	int endTag = immuneIndex+disease.length();
				textArea.append( "\tendTag = " + (endTag-1)+"\n");
				textArea.append("Immune Sys after  treatment = ");
				textArea.append(immuneSys.substring(0,immuneIndex));
				textArea.append("\'" + immuneSys.substring(immuneIndex, endTag));
				textArea.append("\'" + immuneSys.substring(endTag)+"\n");						
			}
		}
	
		while(dIndex < dCount)
		{
			disease = citizen.getDisease(dIndex);
		
			immuneIndex = immuneSys.indexOf(disease);
			if(immuneIndex >= 0)	//citizen now immune to this disease
			{	
				if(GoLconst.DEBUG || GoLconst.DEBUG_DISEASE || GoLconst.DEBUG_DISEASE_TREAT)
				textArea.append("Disease eliminated: " + disease + 
											", matched at index " + immuneIndex+"\n");						

				citizen.removeDisease(dIndex);
				dCount--;
			} 
			else
				dIndex++;
		}
		//Find & infect Neighbors
		if(citizen.getDiseaseCount() > 0)
			infectNeighbors(citizen);
	}

	/**
	 * Randomly selects from disease list and infects each neighbor
	 * @param citizen	who is spreading the disease 
	 */
	private void infectNeighbors(Citizen citizen)
	{
		if( GoLconst.DEBUG || GoLconst.DEBUG_PROGRAM_FLOW )
		{	textArea.append( "\tinfectNeighbors " + citizen.getFamily() + "\n" );	}
		
		int i0, ix, j0, jx;
		int col = citizen.getCol();
		int row = citizen.getRow();
		//Define Search Grid For Soulmate
		i0 = col - 1;	ix = col + 1;
		j0 = row - 1;	jx = row + 1;
		//Ensure Search does not extend beyond grid borders
		if(i0 < 0)	{ i0 = 0; }
		if(j0 < 0)	{ j0 = 0; }
		if(ix >= cellCols) { ix = cellCols - 1; }
		if(jx >= cellRows) { jx = cellRows - 1; }
		
		int randomNum,
			result = 0,
			dCount = citizen.getDiseaseCount();
		String disease="";
		for(int i=i0; i<=ix; i++)
			for(int j=j0; j<=jx; j++)
			{	if( cell[i][j].citizen != null && (col != i && row != j) )
				{	//transmit disease to neighbor
					randomNum = (int)( (float)Math.random() * dCount );
					disease = citizen.getDisease(randomNum);
					result = cell[i][j].citizen.setDisease(disease);
					if(GoLconst.DEBUG || GoLconst.DEBUG_DISEASE || 
										GoLconst.DEBUG_DISEASE_TRANSMIT)
					{	if(result == 1)
							textArea.append(cell[i][j].citizen.getFamily() + 
									" already infected with " + disease + "\n");
						else if(result == 2)
							textArea.append(cell[i][j].citizen.getFamily() + 
									" already immune to " + disease + "\n");
						else
							textArea.append("Infecting " + cell[i][j].citizen.getFamily() + 
									" with " + disease + "\n");					
					}
				}
			}
	}

	/**
	 * Process timekeeper - Tracks time elapsed since the last call to this method.
	 * 
	 * @return	time elapsed since last call.
	 */
	static public long timeElapsed()
	{	long interval, currTime;

		currTime = System.currentTimeMillis();
		if( GoLconst.DEBUG_PROCESS_TIME )
			System.out.println( "Initial time: " + (currTime - startTime) );
		interval = currTime - startTime;
		startTime = currTime;

		return interval;
	}

	/**
	 * Attempt to write simulation data to diskfile - have to find another way of doing this
	 * as Java security does not permit this operation.
	 *
	 * @param	text
	 */
	public void writeText(String text)
	{
		//BufferedOutputStream bos;
		int index = 0, textLen = text.length();


		if( GoLconst.OUTPUT_DESTINATION.equals("SCREEN") )
		{	textArea.append( text ); }
		else
		{	if( !GoLconst.flagOutFileCreated )
			{	try
				{	// Create and attach a simple file input stream to a
					// buffered filter, using the default buffer size of 2048 bytes.
					bos = new BufferedOutputStream(new
										 FileOutputStream("AI_exec.txt"));
					//bos.write( text.charAt(0) );
				}
				catch (Exception e)
				{	System.err.println("Error creating output file: " + e);	}
				GoLconst.flagOutFileCreated = true;
			}
			try
			{	do {	bos.write( text.charAt(index) );	}
				while( index++ > textLen );
			}
			catch (Exception e)
			{	System.err.println("Error writing output file: " + e);
				bos = null;
			}
		}

	}
	
    /**
     * @return
     */
    public int getTotAge()
    {
        return totAge;
    }

    /**
     * @return
     */
    public int getTotLifespan()
    {
        return totLifespan;
    }

    /**
     * @return
     */
    public int getTotSpice()
    {
        return totSpice;
    }

    /**
     * @return
     */
    public int getTotSpMetab()
    {
        return totSpMetab;
    }

    /**
     * @return
     */
    public int getTotSugar()
    {
        return totSugar;
    }

    /**
     * @return
     */
    public int getTotSuMetab()
    {
        return totSuMetab;
    }

    /**
     * @return
     */
    public int getBirthCount()
    {
        return birthCount;
    }

    /**
     * @return
     */
    public int getDeathByDerliction()
    {
        return deathByDerliction;
    }

    /**
     * @return
     */
    public int getDeathByDotage()
    {
        return deathByDotage;
    }

    /**
     * @return
     */
    public int getDeathByStarvation()
    {
        return deathByStarvation;
    }

    /**
     * @return
     */
    public int getDistrAdults()
    {
        return distrAdults;
    }

    /**
     * @return
     */
    public int getDistrChildren()
    {
        return distrChildren;
    }

    /**
     * @return
     */
    public int getDistrEff()
    {
        return distrEff;
    }

    /**
     * @return
     */
    public int getDistrFemale()
    {
        return distrFemale;
    }

    /**
     * @return
     */
    public int getDistrHyp()
    {
        return distrHyp;
    }

    /**
     * @return
     */
    public int getDistrHypSp()
    {
        return distrHypSp;
    }

    /**
     * @return
     */
    public int getDistrHypSu()
    {
        return distrHypSu;
    }

    /**
     * @return
     */
    public int getDistrIneff()
    {
        return distrIneff;
    }

    /**
     * @return
     */
    public int getDistrMale()
    {
        return distrMale;
    }

    /**
     * @return
     */
    public int getDistrRiskAverse()
    {
        return distrRiskAverse;
    }

    /**
     * @return
     */
    public int getDistrRiskTaker()
    {
        return distrRiskTaker;
    }

    /**
     * @return
     */
    public int getDistrSeniors()
    {
        return distrSeniors;
    }

    /**
     * @return
     */
    public int getDistrSlow()
    {
        return distrSlow;
    }

    /**
     * @return
     */
    public int getDistrSlowSp()
    {
        return distrSlowSp;
    }

    /**
     * @return
     */
    public int getDistrSlowSu()
    {
        return distrSlowSu;
    }

    /**
     * @return
     */
    public int getDistrSummer()
    {
        return distrSummer;
    }

    /**
     * @return
     */
    public int getDistrWinter()
    {
        return distrWinter;
    }

    /**
     * @return
     */
    public int getTotVision()
    {
        return totVision;
    }

    /**
     * @return	time slice claimed by food gathering processes.
     */
    public static long getGatherTime()
    {
        return gatherTime;
    }

    /**
     * @return	time slice claimed by mate search & child-birth processes.
     */
    public static long getMateTime()
    {
        return mateTime;
    }

	/**
	 * @return	time slice claimed by bartering processes.
	 */
	public static long getBarterTime()
	{
		return barterTime;
	}

    /**
     * @return	time slice claimed by processes other than gather, mate & barter.
     */
    public static long getOtherTime()
    {
        return otherTime;
    }

    /**
     * @return
     */
    public int getCellCols()
    {
        return cellCols;
    }

    /**
     * @return
     */
    public int getCellRows()
    {
        return cellRows;
    }

	/**
	 * @param i
	 */
	public void resetScoreVars()
	{
		birthCount=0; deathByStarvation=0; deathByDotage=0; deathByDerliction=0; 
			totVision=0; totSugar=0; totSuMetab=0; totSpice=0; totSpMetab=0; totLifespan=0; 
			totAge=0; distrSummer=0; distrWinter=0; distrMale=0; distrFemale=0; 
			distrChildren=0; distrAdults=0; distrSeniors=0; distrHyp=0; distrHypSu=0; 
			distrHypSp=0; distrEff=0; distrIneff=0; distrSlowSu=0; distrSlowSp=0; distrSlow=0; 
			distrRiskTaker=0; distrRiskAverse=0;
	}

    /**
     * @param citizen
     * @param action
     */
    private void setDistrChildAdultSenior(Citizen citizen, int action)
    {
    	if( action == 0 )	//initialise group count
    	{	distrChildren = 0;
        	distrAdults = 0;
        	distrSeniors = 0;
		}
		else if( action == 1 )	//increment relevant group count
		{	int age	= citizen.getAge();
			if( citizen.isMale() )
			{	if( age > GoLconst.MATING_MALE_MAX )
					distrSeniors++;
				else if( age >= GoLconst.MATING_MALE_MIN )
					distrAdults++;
				else distrChildren++;
			}
			else	//Citizen is Female
			{	if( age > GoLconst.MATING_FEMALE_MAX )
					distrSeniors++;
				else if( age >= GoLconst.MATING_FEMALE_MIN )
					distrAdults++;
				else distrChildren++;
			}
		}
    }

    /**
     * @param i
     */
    public void setDistrSeason(Citizen citizen, int i)
    {
		if( i == 0 )
		{
			distrSummer = 0;
			distrWinter = 0;
		}
		else if( i == 1 )
		{
			int boundary = this.cellRows / 2;
			String area1 = ( (getTimePeriod()/100) % 2) == 0 ? "SUMMER" : "WINTER";
			int row = citizen.getRow();
			if( area1.equals("SUMMER") )
				if( row < boundary )	//citizen located in area1
					distrSummer++;
				else distrWinter++;
			else	//area1 equals winter
				if( row < boundary )	//citizen located in area1
					distrWinter++;
				else distrSummer++;
		}
    }

    /**
     * @param i
     */
    public void setTotAge(Citizen citizen, int i)
    {
		if( i == 0 )
			totAge = 0;
		else if( i == 1 )
			totAge += citizen.getAge();
    }

    /**
     * @param i
     */
    public void setTotSugarSpice(Citizen citizen, int i)
    {
		if( i == 0 )
		{	totSpice = 0;
			totSugar = 0;
		}
		else if( i == 1 )
		{	totSpice += citizen.getSpice();
//textArea.append(citizen.showStats("Why?"));
//textArea.append("\n2652Total Sugar = " + getTotSugar() + 
//					"\ncitizen.getSugar = " + citizen.getSugar() + "\n");

			totSugar += citizen.getSugar();

//textArea.append("\n2658Total Sugar = " + getTotSugar() + 
//					"\ncitizen.getSugar = " + citizen.getSugar() + "\n");

		}
    }
	/**
	 * Tabulates statistics to reflect changes to the Sugarscape population.
	 * This method is called when a birth or death event occurs, triggering a 
	 * reevaluation of census data on the Sugarscape.
	 */
	private void setCellspaceStats(Citizen citizen, int event)
	{
		char mVision, mSu, mSp;
		String area1 = ((getTimePeriod() / GoLconst.SEASON_DURATION) % 2) == 0 ?
								"SUMMER" : "WINTER";
		int border	= cellRows / 2;

if(citizen==null)
	textArea.append(citizen.showStats("2667)Starving\t"));
		int currVision = citizen.getVision();
		int currMetabSugar = citizen.getMetabSugar();
		int currMetabSpice = citizen.getMetabSpice();
		
		mVision = currVision >= (0.5 * GoLconst.VISION_MAX)? 'H' : 'L';
		mSu		= currMetabSugar >= (0.5 * GoLconst.METABOLISM_MAX_SUGAR)? 'H':'L';
		mSp		= currMetabSpice >= (0.5 * GoLconst.METABOLISM_MAX_SPICE)? 'H':'L';
			


		if(event == DEATHEVENT)	//death of a citizen
		{
			totVision -= currVision;
			//totSugar -= citizen.getSugar();	
			totSuMetab -= currMetabSugar;
			//totSpice -= citizen.getSpice();
			totSpMetab -= currMetabSpice;
			totLifespan -= citizen.getLifeSpan();
			//totAge -= citizen.getAge();

			if( area1.equals("SUMMER") )
			{	if(citizen.getCol() <= border)
					--distrSummer;
				else
					--distrWinter;
			} 
			else
			{	if(citizen.getCol() <= border)
					--distrWinter;
				else
					--distrSummer;
			} 
			if(citizen.isMale())
			{	--distrMale;
				if(citizen.getAge() < GoLconst.MATING_MALE_MIN)
					--distrChildren;
				else if(citizen.getAge() > GoLconst.MATING_MALE_MAX)
					--distrSeniors;
				else 
					--distrAdults;
			}
			else
			{	distrFemale--;
				if(citizen.getAge() < GoLconst.MATING_FEMALE_MIN)
					--distrChildren;
				else if(citizen.getAge() > GoLconst.MATING_FEMALE_MAX)
					--distrSeniors;
				else 
					--distrAdults;
			}
				
			//Type-wise Distribution of Population
			// HYPER -->  HIGH vision / HIGH sugar metab / HIGH spice metab
			if( mVision == 'H' && mSu == 'H' && mSp == 'H' )
			{	distrHyp--;	}
			
			// HYPER, prefers Sugar --> HIGH vision / HIGH sugar metab / LOW spice metab
			else if( mVision == 'H' && mSu == 'H' && mSp == 'L' )
			{	distrHypSu--;	}
			
			// HYPER, prefers Spice --> HIGH vision / LOW sugar metab / HIGH spice metab
			else if( mVision == 'H' && mSu == 'L' && mSp == 'H' )
			{	distrHypSp--;	}
			
			// EFFICIENT -->  HIGH vision / LOW sugar metab / LOW spice metab
			else if( mVision == 'H' && mSu == 'L' && mSp == 'L' )
			{	distrEff--;	}
			
			// INEfficient -->  LOW vision / HIGH sugar metab / HIGH spice metab
			else if( mVision == 'L' && mSu == 'H' && mSp == 'H' )
			{	distrIneff--;	}
			
			// SLOW -->  LOW vision / LOW sugar metab / LOW spice metab
			else if( mVision == 'L' && mSu == 'L' && mSp == 'L' )
			{	distrSlow--;	}
			
			// SLOW, prefers Sugar --> LOW vision / HIGH sugar metab / LOW spice metab
			else if( mVision == 'L' && mSu == 'H' && mSp == 'L' )
			{	distrSlowSu--;	}
			
			// SLOW, prefers Spice --> LOW vision / LOW sugar metab / HIGH spice metab
			else if( mVision == 'L' && mSu == 'L' && mSp == 'H' )
			{	distrSlowSp--;	}

			if(citizen.getPersona() == 0)	//risk-averse
				distrRiskAverse--;
			else
				distrRiskTaker--;
		}
		else if(event == BIRTHEVENT)
		{
			birthCount++;
			totVision += currVision;
			totSuMetab += currMetabSugar;
			totSpMetab += currMetabSpice;
			totLifespan += citizen.getLifeSpan();

			if( area1.equals("SUMMER") )
			{	if(citizen.getCol() <= border)
					++distrSummer;
				else
					++distrWinter;
			} 
			else
			{	if(citizen.getCol() <= border)
					++distrWinter;
				else
					++distrSummer;
			} 
			if(citizen.isMale())
			{	++distrMale;
				if(citizen.getAge() < GoLconst.MATING_MALE_MIN)
					++distrChildren;
				else if(citizen.getAge() > GoLconst.MATING_MALE_MAX)
					++distrSeniors;
				else 
					++distrAdults;
			}
			else
			{	distrFemale++;
				if(citizen.getAge() < GoLconst.MATING_FEMALE_MIN)
					++distrChildren;
				else if(citizen.getAge() > GoLconst.MATING_FEMALE_MAX)
					++distrSeniors;
				else 
					++distrAdults;
			}
			// HYPER -->  HIGH vision / HIGH sugar metab / HIGH spice metab
			if( mVision == 'H' && mSu == 'H' && mSp == 'H' )
			{	distrHyp++;	}
			
			// HYPER, prefers Sugar --> HIGH vision / HIGH sugar metab / LOW spice metab
			else if( mVision == 'H' && mSu == 'H' && mSp == 'L' )
			{	distrHypSu++;	}
			
			// HYPER, prefers Spice --> HIGH vision / LOW sugar metab / HIGH spice metab
			else if( mVision == 'H' && mSu == 'L' && mSp == 'H' )
			{	distrHypSp++;	}
			
			// EFFICIENT -->  HIGH vision / LOW sugar metab / LOW spice metab
			else if( mVision == 'H' && mSu == 'L' && mSp == 'L' )
			{	distrEff++;	}
			
			// INEfficient -->  LOW vision / HIGH sugar metab / HIGH spice metab
			else if( mVision == 'L' && mSu == 'H' && mSp == 'H' )
			{	distrIneff++;	}
			
			// SLOW -->  LOW vision / LOW sugar metab / LOW spice metab
			else if( mVision == 'L' && mSu == 'L' && mSp == 'L' )
			{	distrSlow++;	}
			
			// SLOW, prefers Sugar --> LOW vision / HIGH sugar metab / LOW spice metab
			else if( mVision == 'L' && mSu == 'H' && mSp == 'L' )
			{	distrSlowSu++;	}
			
			// SLOW, prefers Spice --> LOW vision / LOW sugar metab / HIGH spice metab
			else if( mVision == 'L' && mSu == 'L' && mSp == 'H' )
			{	distrSlowSp++;	}

			if(citizen.getPersona() == 0)	//risk-averse
				distrRiskAverse++;
			else
				distrRiskTaker++;
		}
	}

	/**
	 * Tallies cultural group membership and initializes color array determining 
	 * color of individual groups.
	 * @see	GoLconst#CITIZEN_GROUP_INTERVAL	CITIZEN_GROUP_INTERVAL
	 * @see	GoLconst#CITIZEN_CULTURE_TAG	CITIZEN_CULTURE_TAG
	 */
	public static void setDistrGroupsInit()
	{	Citizen.setDistrGroupsInit();	}



	/**
	 * @param citizen
	 * @param action
	 */
	public void setDistrGroups()
	{
		//Citizen citizen;
		int gCount = groupCount - 1;
		for(int i=0; i<gCount; i++)
		{	rangeArr[i] = GoLconst.CITIZEN_GROUP_INTERVAL[i];
			rangeTally[i] = 0;
		}
		rangeArr[gCount] = GoLconst.CITIZEN_CULTURE_TAG;
		rangeTally[gCount] = 0;
		
		if(GoLconst.DEBUG || GoLconst.DEBUG_CRITICAL_ERROR)
			if(rangeName.length < groupCount)
				textArea.append(
					"setDistrGroups(): Not enough names for cultural groups\n");

		int index = 0;
		int zenCount = zenList.size();
		int memeCount;
		while( index < zenCount )
		{	citizen = (Citizen)zenList.get(index++);
			memeCount = citizen.getMemeTally();
			for(int i=0; i<=groupCount; i++)
			{	if(memeCount <= rangeArr[i])
				{	rangeTally[i]++;
					break;
				}
			}
		}
	}

	/**
	 * Calculate and display detailed stats on Cellspace population.
	 * Includes:
	 * <UL>
	 * <LI> Total population, births, deaths
	 * <LI> Population distribution by climate
	 * <LI> Population distribution by age group
	 * <LI> Population density
	 * <LI> Averages for various citizen characteristics like vision metabolism etc.
	 * <LI> Population distribution by needs/abilities*. 
	 * </UL><br>
	 * Population distribution by needs/abilities is an arbitrary attempt
	 * to group citizens based on their characteristics and preferences.
	 * The classification comprises eight categories as follows:
	 * <UL>
	 * <LI> <B>Hyper</B>	citizens with high vision and metabolism values.
	 * <LI> <B>Hyper, prefer Sugar</B>	citizens with high vision & sugar metabolism 
	 * but low spice metabolism. These are said to prefer sugar.
	 * <LI> <B>Hyper, prefer Spice</B>	citizens with high vision & spice metabolism 
	 * but low sugar metabolism.
	 * <LI> <B>Efficient</B>	citizens with high vision and low metabolism values.
	 * <LI> <B>Inefficient</B>	citizens with low vision and high metabolism values.
	 * <LI> <B>Slow, prefer Sugar</B>	citizens with low vision & spice metabolism 
	 * but high sugar metabolism.
	 * <LI> <B>Slow, prefer Spice</B>	citizens with low vision & sugar metabolism 
	 * but high spice metabolism.
	 * <LI> <B>Slow</B>	citizens with low vision and metabolism values.
	 * </UL>
	 * 
	 * @param	infoType	represents amount of detail requested.
	 */
	public  void showPopulationStats(int infoType)
	{
		if( infoType == 2 )
		{	dead += deathList.size();
			born += birthList.size();
			return;
		}

		Citizen citizen;
		int boundary = this.cellRows / 2;
		String area1 = ( (getTimePeriod()/100) % 2) == 0 ? "SUMMER" : "WINTER";
		int row, currVision, currMetabSugar, currMetabSpice, age;
		float currSugar, currSpice; 
		float visionTotal = 0, metabTotalSugar = 0, metabTotalSpice = 0,
				sugarCitizens = 0, spiceCitizens = 0;
		int index = 0, summerPop = 0, winterPop = 0;
		int children = 0, adults = 0, seniors = 0;
		char mVision, mSu, mSp;
		// **Citizen Types**
		int hyper = 0, 		//Hyper in all attributes
			hypSu = 0,  	//Hyper & needs more sugar
			hypSp = 0, 		//Hyper & needs more spice
			eff = 0, 		//Efficient gatherer & consumer
			inEff = 0,		//Inefficient gatherer & consumer
			slow = 0,		//Slow in all attributes
			sloSu = 0,  	//Slow & needs more sugar
			sloSp = 0;  	//Slow & needs more spice

		if( zenList.isEmpty() )
		{	textArea.append( "No population stats, list is empty()\n" );
			return;
		}

		ListIterator zenIter = zenList.listIterator();
		while( zenIter.hasNext() )
		{	zenIter.next();
			citizen =  (Citizen) zenList.get(index++);

			row 			= citizen.getRow();
			currVision 		= citizen.getVision();
			currMetabSugar 	= citizen.getMetabSugar();
			currSugar 		= citizen.getSugar();
			currMetabSpice 	= citizen.getMetabSpice();
			currSpice 		= citizen.getSpice();
			age				= citizen.getAge();

			if( infoType == 1 )
				textArea.append( citizen.showStats("") );

			//Population averages
			visionTotal += currVision;
			metabTotalSugar += currMetabSugar;
			sugarCitizens += currSugar;
			metabTotalSpice += currMetabSpice;
			spiceCitizens += currSpice;

			//Geographic Distribution
			if( area1.equals("SUMMER") )
				if( row < boundary )	//citizen located in area1
					summerPop++;
				else winterPop++;
			else	//area1 equals winter
				if( row < boundary )	//citizen located in area1
					winterPop++;
				else summerPop++;

			//Age-wise distribution
			if( citizen.isMale() )
			{	if( age > GoLconst.MATING_MALE_MAX )
					seniors++;
				else if( age >= GoLconst.MATING_MALE_MIN )
					adults++;
				else children++;
			}
			else	//Citizen is Female
			{
				if( age > GoLconst.MATING_FEMALE_MAX )
					seniors++;
				else if( age >= GoLconst.MATING_FEMALE_MIN )
					adults++;
				else children++;
			}

			//Type-wise Distribution of Population
			mVision = currVision >= (0.5 * GoLconst.VISION_MAX)? 'H' : 'L';
			mSu		= currMetabSugar >= (0.5 * GoLconst.METABOLISM_MAX_SUGAR)? 'H':'L';
			mSp		= currMetabSpice >= (0.5 * GoLconst.METABOLISM_MAX_SPICE)? 'H':'L';

			// HYPER -->  HIGH vision / HIGH sugar metab / HIGH spice metab
			if( mVision == 'H' && mSu == 'H' && mSp == 'H' )
			{	hyper++;	}

			// HYPER, prefers Sugar --> HIGH vision / HIGH sugar metab / LOW spice metab
			else if( mVision == 'H' && mSu == 'H' && mSp == 'L' )
			{	hypSu++;	}

			// HYPER, prefers Spice --> HIGH vision / LOW sugar metab / HIGH spice metab
			else if( mVision == 'H' && mSu == 'L' && mSp == 'H' )
			{	hypSp++;	}

			// EFFICIENT -->  HIGH vision / LOW sugar metab / LOW spice metab
			else if( mVision == 'H' && mSu == 'L' && mSp == 'L' )
			{	eff++;	}

			// INEfficient -->  LOW vision / HIGH sugar metab / HIGH spice metab
			else if( mVision == 'L' && mSu == 'H' && mSp == 'H' )
			{	inEff++;	}

			// SLOW -->  LOW vision / LOW sugar metab / LOW spice metab
			else if( mVision == 'L' && mSu == 'L' && mSp == 'L' )
			{	slow++;	}

			// SLOW, prefers Sugar --> LOW vision / HIGH sugar metab / LOW spice metab
			else if( mVision == 'L' && mSu == 'H' && mSp == 'L' )
			{	sloSu++;	}

			// SLOW, prefers Spice --> LOW vision / LOW sugar metab / HIGH spice metab
			else if( mVision == 'L' && mSu == 'L' && mSp == 'H' )
			{	sloSp++;	}

		}
		zenIter = null;

		textArea.append( "+--------------------------------------------------------------+\n" );
		textArea.append( "|Grid size\t\t: "  	+ cellCols + "x" + cellRows + "\n" );
		textArea.append( "|Population\t\t: " 	+ population + "\t(Births : " + born
												+ "\tDeaths : " + dead + ")\n" );
		textArea.append( "|Climate-wise distribution\n|\tSummer region\t: " 	+ summerPop
						+ "\n\tWinter region\t: " 	+ winterPop + "\n"  );
		textArea.append( "|Age-wise distribution\n|\tChildren\t\t: " 	+ children
						+ "\n|\tAdults\t\t: " 	+ adults
						+ "\n|\tSeniors\t\t: " + seniors + "\n"  );

		textArea.append( "|Cells/Citizen\t: " + GoLconst.customFormat("###.##", 
										((float)(cellCols*cellRows)/population)) + "\n" );
		textArea.append( "|Avg vision\t: " + GoLconst.customFormat( "###.##", 
										(visionTotal/population) ) + "\n" );
		textArea.append( "|Avg Sugar Metablism\t: " + GoLconst.customFormat( "###.##", 
										(metabTotalSugar/population) ) + "\n" );
		textArea.append( "|Sugar/citizen\t: " + GoLconst.customFormat( "###.##", 
										(sugarCitizens/population) ) + "\n" );
		textArea.append( "|Avg Spice metabolism\t: " + GoLconst.customFormat( "###.##", 
										(metabTotalSpice/population) ) + "\n" );
		textArea.append( "|Sugar/citizen\t: " + GoLconst.customFormat( "###.##", 
										(spiceCitizens/population) ) + "\n" );
		textArea.append( "|Type-wise distribution\n"
					+"|\tHyper\t\t: " + hyper + "\n"
					+"|\tHyper, prefer Sugar\t: " + hypSu + "\n"
					+"|\tHyper, prefer Spice\t: " + hypSp + "\n"
					+"|\tEfficient\t\t: " + eff + "\n"
					+"|\tInefficient\t\t: " + inEff + "\n"
					+"|\tSlow, prefer Sugar\t: " + sloSu + "\n"
					+"|\tSlow, prefer Spice\t: " + sloSp + "\n"
					+"|\tSlow\t\t: " + slow + "\n"	);
		for(int gCount=0; gCount<groupCount; gCount++)
		{	if(rangeTally[gCount]>0)
				textArea.append(rangeName[gCount] + " = " + rangeTally[gCount] + "\n");
		}
		textArea.append( "+---------------------------------------------------------------+\n" );
					

	}

	/**
	 * @param	message
	 * @param	citizen
	 * @deprecated	this method has benn made part of the Citizen object.
	 */
	public  void showCitizenStats( String message, Citizen citizen )
	{	String family = citizen.getFamily();
		String spacer = "", spacer2="";

		if( citizen.getCol() < 10) 			{	spacer += "  ";	}
		else if( citizen.getCol() < 100) 	{	spacer += " ";	}
		if( citizen.getRow() < 10) 			{	spacer += "  ";	}
		else if( citizen.getRow() < 100) 	{	spacer += " ";	}

		int spaceCntr = (15 - family.length() );
		while( spaceCntr-- > 0)
			spacer2 += "  ";

		textArea.append(  message + spacer + citizen.getID() + ") " + family.trim()
			+ spacer2 + "[" + citizen.getCol() + "][" + citizen.getRow() + "]\t"
			+ citizen.getVision() + "V  "
			+ citizen.getSugar() + "Su/" + citizen.getMetabSugar() + "M/"
				+ citizen.getInheritSugar() + "I\t"
			+ citizen.getSpice() + "Sp/" + citizen.getMetabSpice() + "M/"
				+ citizen.getInheritSpice() + "I\t"
			+ (citizen.isMale() ? "M" : "Fem") + "ale  "
			+ citizen.getAge() + " yrs (" + citizen.getLifeSpan() + "), "
			+ ", Rank " + citizen.getRanking() + ", MRS " + citizen.getMRS()+ "\n" );
	}

	/**
	 * @param	message
	 * @param	col
	 * @param	row
	 */
	public  String showCitizenStats( String message, int col, int row )
	{	Citizen citizen = cell[col][row].citizen;
		return citizen.showStats(message);
	}

	/**
	 * @param	message
	 * @param	col
	 * @param	row
	 * @deprecated	this method has benn made part of the Citizen object.
	 */
	/*public  void showCitizenStats( String message, int col, int row )
	{	Citizen citizen = cell[col][row].citizen;
		if( citizen == null )
		{	textArea.append("No citizen at location (" + col+","+row + ")\n");
			return;
		}

		String family = cell[col][row].citizen.getFamily();
		String spacer = "", spacer2 = "";
		if( citizen.getCol() < 10) 			{	spacer += "  ";	}
		else if( citizen.getCol() < 100) 	{	spacer += " ";	}
		if( citizen.getRow() < 10) 			{	spacer += "  ";	}
		else if( citizen.getRow() < 100) 	{	spacer += " ";	}

		int spaceCntr = (15 - family.length() );
		while( spaceCntr-- > 0)
			spacer2 += "  ";	//adjusting for varying length of family name

		textArea.append( message + spacer + citizen.getID() + ") " + family.trim()
			+ spacer2 + "[" + citizen.getCol() + "][" + citizen.getRow() + "]\t"
			+ citizen.getVision() + "V  "
			+ citizen.getSugar() + "Su/" + citizen.getMetabSugar() + "M/"
				+ citizen.getInheritSugar() + "I \t"
			+ citizen.getSpice() + "Sp/" + citizen.getMetabSpice() + "M/"
				+ citizen.getInheritSpice() + "I \t"
			+ (citizen.isMale() ? "M" : "Fem") + "ale  "
			+ citizen.getAge() + " yrs (" + citizen.getLifeSpan() + "), "
			+ "Rank " + citizen.getRanking() + "\n" );	
	}*/



	/**
	 * Display the sugar distribution on the Sugarscape.
	 * 
	 * @param	startCol	column value for top-left cell of grid.
	 * @param	startRow	row value for top-left cell of grid.
	 * @param	endCol	column value for bottom-right cell of grid.
	 * @param	endRow	row value for bottom-right cell of grid.
	 */
	public  void showSugarStats(int startCol,int startRow,int endCol,int endRow)
	{	String dataStream;
		dataStream = "\n    ";

		for(int i = startCol; i < endCol; i++)
			if(i < 10)	{	dataStream  +=  " " + i + " ";	}
			else dataStream +=  i + " ";
		dataStream += "\n";

		for(int i = startCol; i <= endCol; i++)
			dataStream += "---";
		dataStream += "\n";

		for (int i=startRow;i<endRow;i++)
		{	if(i<10) dataStream += "  " + i + "|";
			else dataStream += " " + i + "|";

			for (int j=startCol;j<endCol;j++)
				dataStream += " " + cell[j][i].getSugar() + " ";
			dataStream += "\n";
		}
		textArea.append( dataStream );
	}
	
	/**
	 * Display the spice distribution on the Sugarscape.
	 * 
	 * @param	startCol	column value for top-left cell of grid.
	 * @param	startRow	row value for top-left cell of grid.
	 * @param	endCol	column value for bottom-right cell of grid.
	 * @param	endRow	row value for bottom-right cell of grid.
	 */
	public  void showSpiceStats(int startCol,int startRow,int endCol,int endRow)
	{	String dataStream;
		dataStream = "\n    ";

		for(int i=startCol; i<endCol; i++)
			if(i<10)	{	dataStream  +=  " " + i + " ";	}
			else dataStream +=  i + " ";
		dataStream += "\n";

		for(int i=startCol; i<=endCol; i++)
			dataStream += "---";
		dataStream += "\n";

		for (int i=startRow;i<endRow;i++)
		{	if(i<10) dataStream += "  " + i + "|";
			else dataStream += " " + i + "|";

			for (int j=startCol;j<endCol;j++)
				dataStream += " " + cell[j][i].getSpice() + " ";
			dataStream += "\n";
		}
		textArea.append( dataStream );
	}


	/**
	 * Calls the relevant Cell method to display stats for a given cell.
	 * 
	 * @param	col	column location of the cell.
	 * @param	row	row location of the cell.
	 * @return	string, containing cell stats.
	 */
	public String showCellStats(int col, int row)
	{	return cell[col][row].showStats(col, row);		}

	/**
	 * Displays time slices used by various broadly defined processes.
	 * 
	 */
	public  void showProcessStats()
	{	textArea.append( "Gather=" + gatherTime + "\tMating=" + mateTime
					+ "\tOther=" + otherTime + "\n" );
	}

	/**
	 * Display settings for all DEBUG switches.
	 * 
	 */
	public  void showDebugSettings()
	{	textArea.append("\nDEBUG_CRITICAL_ERROR = "
					+ GoLconst.DEBUG_CRITICAL_ERROR + "\n");
		textArea.append("DEBUG_CMD_FEEDBACK = "
					+ GoLconst.DEBUG_CMD_FEEDBACK + "\n");
		textArea.append("DEBUG_PROGRAM_FLOW = "
										+ GoLconst.DEBUG_PROGRAM_FLOW + "\n");
		textArea.append("DEBUG_CITIZEN_BIRTH = "
										+ GoLconst.DEBUG_CITIZEN_BIRTH + "\n");
		textArea.append("DEBUG_CITIZEN_DEATH = "
										+ GoLconst.DEBUG_CITIZEN_DEATH + "\n");
		textArea.append("DEBUG_SEARCH_SUGAR = "
										+ GoLconst.DEBUG_SEARCH_SUGAR + "\n");
		textArea.append("DEBUG_SUGAR_PRODUCTION = "
										+ GoLconst.DEBUG_SUGAR_PRODUCTION + "\n");
		textArea.append("DEBUG_MATING_SEARCH = "
										+ GoLconst.DEBUG_MATING_SEARCH + "\n");
		textArea.append("DEBUG_MATING_SELECTION = "
										+ GoLconst.DEBUG_MATING_SELECTION + "\n");
		textArea.append("DEBUG_MATING_BIRTH = "
										+ GoLconst.DEBUG_MATING_BIRTH + "\n");
		textArea.append("DEBUG_MATING = " + GoLconst.DEBUG_MATING + "\n");
		textArea.append("DEBUG_POLLUTION = " + GoLconst.DEBUG_POLLUTION + "\n");
		textArea.append("DEBUG_PROCESS_TIME = " + GoLconst.DEBUG_PROCESS_TIME + "\n");
		textArea.append("DEBUG_INHERITANCE = " + GoLconst.DEBUG_INHERITANCE + "\n");
		textArea.append("DEBUG_BARTER = " + GoLconst.DEBUG_BARTER + "\n");
		textArea.append("DEBUG_BARTER_SORT = " + GoLconst.DEBUG_BARTER_SORT + "\n");
		textArea.append("DEBUG_BARTER_EXCHANGE = " + GoLconst.DEBUG_BARTER_EXCHANGE + "\n");
		textArea.append("DEBUG = " + GoLconst.DEBUG + "\n");
	}
	
    /**
     * @return
     */
    public int getRangeTally(int groupIndex)
    {
        return rangeTally[groupIndex];
    }

}