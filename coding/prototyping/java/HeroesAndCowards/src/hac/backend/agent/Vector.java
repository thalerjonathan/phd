package hac.backend.agent;

import hac.backend.Utils;

/**
 * Created by jonathan on 05/12/16.
 */
public class Vector {
    private double x;
    private double y;

    public Vector(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public Vector(Vector v) {
        this.x = v.x;
        this.y = v.y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public Vector multiply(double d) {
        return new Vector(x*d, y*d);
    }

    public Vector delta(Vector p) {
        return new Vector(p.x - this.x, p.y - this.y);
    }

    public Vector add(Vector p) {
        return new Vector(this.x + p.x, this.y + p.y);
    }

    public Vector sub(Vector p) {
        return new Vector(this.x - p.x, this.y - p.y);
    }

    public double length() {
        return Math.sqrt(this.x * this.x + this.y * this.y);
    }

    public Vector norm() {
        double len = this.length();
        if (len == 0) {
            return new Vector(0, 0);
        }

        return new Vector( this.x / len, this.y / len);
    }

    public Vector clip(double min, double max) {
        double x = Math.max( min, Math.min( this.x, max ) );
        double y = Math.max( min, Math.min( this.y, max ) );

        return new Vector(x, y);
    }

    public Vector wrap() {
        double x = this.x;
        double y = this.y;

        if ( x > 1.0 )
            x = 0.0;
        else if ( x < 0.0 )
            x = 1.0;

        if ( y > 1.0 )
            y = 0.0;
        else if ( y < 0.0 )
            y = 1.0;

        return new Vector(x, y);
    }

    @Override
    public String toString() {
        return "Vector{" +
                "x=" + x +
                ", y=" + y +
                '}';
    }
}
