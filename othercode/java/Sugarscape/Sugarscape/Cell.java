package Sugarscape;

import java.awt.Graphics;
import java.awt.Color;
import java.lang.Math;

/**
 * This is a component of the Sugarscape object. The Sugarscape is a 2D grid of 
 * cell objects. The number of instances is simply the product of the number of rows
 * and columns in the grid.
 * The object is created when the Sugarscape is first instantiated. Cells may be added
 * or removed as the grid is resized dynamically by the user.
 * 
 * @author	abraham kannankeril
 */
class Cell
{
    float sugar;
    int maxSugar;
    float spice;
    int maxSpice;
    float pollution;
    /**
     * Citizen object container within the cell. Initialized to null, when no citizen is 
     * present.
     * 
     * @see	Citizen
     */
    public Citizen citizen;

    /**
     * Constructor object, initializes quantities of sugar and spice within the cell.
     * 
     */
    public Cell()
    {
        setSugar(-1f); //generate random sugar value
        setSpice(-1f);
        pollution = 0.0f;
        citizen = null;
    }

    /**
     * Initializes the quantity of sugar in a cell to a pre-specified or random amount.
     * If the parameter is greater than zero, stores that value as the new quantity of sugar 
     * in the cell; 
     * if the parameter equals zero, signifies that the sugar is being harvested. Calls the 
     * setPollution method to calculate pollution caused by harvesting, then resets sugar 
     * quantity to zero;
     * if the parameter is less than zero, then randomly determines whether cell is to contain 
     * sugar or be barren of sugar. The probability for this is determined by the constant
     * GoLconst.GRID_FERTILITY_SUGAR. If it determined that the cell will grow sugar, then the 
     * actual quantity is randomly determined within a range of 0 - GoLconst.SUGAR_MAX_CELL.
     * 
     * @param	quantity	value that can be positive, 0 or -1.
     */
    public void setSugar(float quantity)
    {
        if( sugar == -1 )
        	return;	//barren cell, cannot change sugar
        if (quantity > 0)
        {    if (GoLconst.LIMIT_CELL_SUGAR)
                sugar = (sugar + quantity) < GoLconst.SUGAR_MAX_CELL
                        ? sugar + quantity
                        : GoLconst.SUGAR_MAX_CELL;
            else
                sugar += quantity;
		}
        else if (quantity == 0) //sugar gathered
        {
            if (GoLconst.INITIATE_POLLUTION)
                setPollution(1); //pollution generated from gathering
            sugar = 0f;
        } else  if (quantity < 0)
        {	if (Math.random() > GoLconst.GRID_FERTILITY_SUGAR)
	            sugar = -1;  //cell remains barren
	        else
	            sugar = (float) (Math.random() * GoLconst.SUGAR_MAX_CELL);
        }
    }

    /**
     * Initializes the quantity of spice in a cell to a pre-specified or random amount.
     * If the parameter is greater than zero, stores that value as the new quantity of spice 
     * in the cell; 
     * if the parameter equals zero, signifies that the spice is being harvested. Calls the 
     * setPollution method to calculate pollution caused by harvesting, then resets spice 
     * quantity to zero;
     * if the parameter is less than zero, then randomly determines whether cell is to contain 
     * spice or be barren. The probability for this is determined by the constant
     * GoLconst.GRID_FERTILITY_SPICE. If it determined that the cell will grow spice, then the 
     * actual quantity is randomly determined within a range of 0 - GoLconst.SPICE_MAX_CELL.
     * 
     * @param	quantity	value that can be positive, 0 or -1.
     */
    public void setSpice(float quantity)
    {
		if( spice == -1 )
			return;	//barren cell, cannot change spice
        if (quantity > 0f)
        {
            if (GoLconst.LIMIT_CELL_SPICE)
                spice = (spice + quantity) < GoLconst.SPICE_MAX_CELL
                        ? spice + quantity
                        : GoLconst.SPICE_MAX_CELL;
            else
                spice += quantity;
        } else if (quantity == 0f) //sugar gathered
        {	if (GoLconst.INITIATE_POLLUTION)
                setPollution(2); //pollution generated from gathering
            spice = 0f;
        } else  if (quantity < 0)
        {	if (Math.random() > GoLconst.GRID_FERTILITY_SPICE)
	            spice = -1; 
	        else
	            spice = (float) (Math.random() * GoLconst.SPICE_MAX_CELL);
        }
    }

    /**
     * Recalculates pollution for the cell. If the parameter equals 1, then determines the 
     * pollution caused by the harvesting of sugar in the cell. Each unit harvested 
     * contributes GoLconst.POLLUTION_PRODUCTION_SUGAR amount of pollution;
     * If the parameter equals 2, then determines the pollution caused by the harvesting of 
     * spice in the cell. Each unit harvested contributes GoLconst.POLLUTION_PRODUCTION_SPICE 
     * amount of pollution;
     * If the parameter equals 0, then pollution dispersal process is executed. Pollution is 
     * reduced by amount specified in GoLconst.POLLUTION_DISPERSION_UNIT. Pollution dispersal 
     * occurs once per cycle / time period.
     * 
     * @param	actionFlag
     */
    public void setPollution(int actionFlag)
    {
        if (actionFlag == 1 && sugar > 0) //sugar being gathered
            pollution += (float) (GoLconst.POLLUTION_PRODUCTION_SUGAR * sugar);
        else if (actionFlag == 2 && spice > 0) //spice being gathered
            pollution += (float) (GoLconst.POLLUTION_PRODUCTION_SPICE * spice);
        else if (actionFlag == 0)	//disperse pollution - actionFlag == 0
            pollution = (pollution >= GoLconst.POLLUTION_DISPERSION_UNIT)
                    ? pollution - GoLconst.POLLUTION_DISPERSION_UNIT : 0;
    }

    /**
     * Returns the current stock of sugar in the cell.
     */
    public float getSugar()
    {	return sugar;    }
    /**
     * Returns the current stock of spice in the cell.
     */
    public float getSpice()
    {	return spice;    }
    /**
     * Returns the current level of pollution in the cell.
     */
    public float getPollution()
    {	return pollution;    }

    /**
     * Returns false when cell is occupied by a citizen.
     */
    public boolean notOccupied()
    {	return (citizen == null);    }

    /**
     * Updates the Cell background of an empty cell. Also indicates the quantity of both 
     * goods graphically in the form of horizontal bars.
     * The display can be configured as two or three-dimensional.
     * 
     * @param	g	handle to the graphics object - used to draw the grid components
     * including cells and citizens.
     * @param	x	axis coordinate identifying the top left corner of the cell to be redrawn.
     * @param	y	axis coordinate identifying the top left corner of the cell to be redrawn.
     * @param	cellSize	Each cell is a square, this defines the length of one side in pixels.
     * @see	GoLconst#CELL_3D
     */
    public void paintCell(Graphics g, int x, int y, int cellSize)
    {
        float mBarUnit = cellSize / (GoLconst.LIMIT_CELL_SUGAR
                    			&& GoLconst.LIMIT_CELL_SPICE
                        ? GoLconst.SUGAR_MAX_CELL + GoLconst.SPICE_MAX_CELL
                        : sugar + spice);
        int mSuBar = Math.round((int) (mBarUnit * sugar));
        int mSpBar = Math.round((int) (mBarUnit * spice));
        
		String area1 = ((GoLconst.timePeriod / GoLconst.SEASON_DURATION) % 2) == 0 ?
				"SUMMER" : "WINTER";
		int border	= GoLconst.GRID_ROWS / 2;
        int mBgColor = GoLconst.CELL_BG_WINTER;

		if(area1 == "SUMMER")
		{
	        if(y < border)
		        mBgColor = GoLconst.CELL_BG_SUMMER;
			else
				mBgColor = GoLconst.CELL_BG_WINTER;
		}
		else if(area1 == "WINTER")
		{
	        if(y < border)
		        mBgColor = GoLconst.CELL_BG_WINTER;
			else
				mBgColor = GoLconst.CELL_BG_SUMMER;
		}

			       
            
        int mFgColor = GoLconst.CELL_BAR;

        if (GoLconst.CELL_3D)
        {
            int mStep = (255 - mBgColor) / cellSize;
            for (int i = cellSize - 1; i > 0; i--)
            {
                g.setColor( new Color( mBgColor + (i * mStep),
                        mBgColor + (i * mStep), mBgColor + (i * mStep)));
                // **Drawing Line on Left-side of Cell**
                g.drawLine( x * cellSize + i, y * cellSize,
                    x * cellSize + i, y * cellSize + cellSize);
                // **Drawing Line on Right-side of Cell**
                g.drawLine( x * cellSize + cellSize - i, y * cellSize,
                    x * cellSize + cellSize - i, y * cellSize + cellSize);
            }
        }
        g.setColor(new Color(mFgColor, mFgColor, mFgColor));
        // **Draw Sugar Bar**
        g.fillRect(x * cellSize, y * cellSize, mSuBar, cellSize);
        // **Draw Spice Bar**
        g.fillRect( x * cellSize + cellSize - mSpBar, y * cellSize,
            mSpBar, cellSize);

        g.setColor(new Color(mBgColor, mBgColor, mBgColor));
    }
    
    /**
     * Returns a string describing the sugar, spice and pollution levels within the cell.
     * 
     * @param	col	displays stats for cell at column identified by this value
     * @param	row	displays stats for cell at row identified by this value
     */
    public String showStats(int col, int row)
    {
        String dataStream = "[" + col + "][" + row + "]\t"
                + GoLconst.customFormat("###.##", sugar) + "Su, " 
                + GoLconst.customFormat("###.##", spice) + "Sp, " 
                + GoLconst.customFormat("###.##", pollution) + "P\t";
        if (citizen != null)
            dataStream += citizen.showStats("");
        dataStream += "\n";
        return dataStream;
    }
    /**
     * @return
     */
    public Citizen getCitizen()
    {
        return citizen;
    }

    /**
     * @return
     */
    public int getMaxSpice()
    {
        return maxSpice;
    }

    /**
     * @return
     */
    public int getMaxSugar()
    {
        return maxSugar;
    }

    /**
     * @param citizen
     */
    public void setCitizen(Citizen citizen)
    {
        this.citizen = citizen;
    }

    /**
     * @param i
     */
    public void setMaxSpice(int i)
    {
        maxSpice = i;
    }

    /**
     * @param i
     */
    public void setMaxSugar(int i)
    {
        maxSugar = i;
    }

}