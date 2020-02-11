/*
 * Smallest enclosing circle - Library (C++)
 *
 * Copyright (c) 2018 Project Nayuki
 * https://www.nayuki.io/page/smallest-enclosing-circle
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program (see COPYING.txt and COPYING.LESSER.txt).
 * If not, see <http://www.gnu.org/licenses/>.
 */

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <random>
#include "rcpp_smallest_circle.h"

using std::size_t;
using std::vector;
using std::max;
using std::min;


// Members of Point

Point::Point()
{

}

Point::Point(double x, double y)
    :x(x), y(y)
{

}

Point Point::subtract(const Point &p) const {
    return Point{x - p.x, y - p.y};
}

double Point::distance(const Point &p) const {
    return std::hypot(x - p.x, y - p.y);
}

double Point::cross(const Point &p) const {
    return x * p.y - y * p.x;
}


// Members of Circle

// static constants:
const Circle Circle::INVALID{Point{0, 0}, -1};
const double Circle::MULTIPLICATIVE_EPSILON = 1 + 1e-14;

bool Circle::contains(const Point &p) const {
    return c.distance(p) <= r * MULTIPLICATIVE_EPSILON;
}

bool Circle::contains(const vector<Point> &ps) const {
    for (const Point &p : ps) {
        if (!contains(p))
            return false;
    }
    return true;
}

// Smallest enclosing circle algorithm

static Circle makeSmallestEnclosingCircleOnePoint (const std::vector<Point> &points,
                                                   size_t end, const Point &p);
static Circle makeSmallestEnclosingCircleTwoPoints(const std::vector<Point> &points,
                                                   size_t end, const Point &p,
                                                   const Point &q);
static std::default_random_engine randGen((std::random_device())());

// Initially: No boundary points known
Circle makeSmallestEnclosingCircle(const vector<Point> &points) {
    // Clone list to preserve the caller's data, randomize order
    vector<Point> shuffled = points;
    std::shuffle(shuffled.begin(), shuffled.end(), randGen);

    // Progressively add points to circle or recompute circle
    Circle c = Circle::INVALID;
    for (size_t i = 0; i < shuffled.size(); i++) {
        const Point &p = shuffled.at(i);
        if (c.r < 0 || !c.contains(p))
            c = makeSmallestEnclosingCircleOnePoint(shuffled, i + 1, p);
    }
    return c;
}

// One boundary point known
static Circle makeSmallestEnclosingCircleOnePoint(const vector<Point> &points, size_t end, const Point &p) {
    Circle c{p, 0};
    for (size_t i = 0; i < end; i++) {
        const Point &q = points.at(i);
        if (!c.contains(q)) {
            if (c.r == 0)
                c = makeDiameter(p, q);
            else
                c = makeSmallestEnclosingCircleTwoPoints(points, i + 1, p, q);
        }
    }
    return c;
}

// Two boundary points known
static Circle makeSmallestEnclosingCircleTwoPoints(const vector<Point> &points, size_t end, const Point &p, const Point &q) {
    Circle circ = makeDiameter(p, q);
    Circle left  = Circle::INVALID;
    Circle right = Circle::INVALID;

    // For each point not in the two-point circle
    Point pq = q.subtract(p);
    for (size_t i = 0; i < end; i++) {
        const Point &r = points.at(i);
        if (circ.contains(r))
            continue;

        // Form a circumcircle and classify it on left or right side
        double cross = pq.cross(r.subtract(p));
        Circle c = makeCircumcircle(p, q, r);
        if (c.r < 0)
            continue;
        else if (cross > 0 && (left.r < 0 || pq.cross(c.c.subtract(p)) > pq.cross(left.c.subtract(p))))
            left = c;
        else if (cross < 0 && (right.r < 0 || pq.cross(c.c.subtract(p)) < pq.cross(right.c.subtract(p))))
            right = c;
    }

    // Select which circle to return
    if (left.r < 0 && right.r < 0)
        return circ;
    else if (left.r < 0)
        return right;
    else if (right.r < 0)
        return left;
    else
        return left.r <= right.r ? left : right;
}

Circle makeDiameter(const Point &a, const Point &b) {
    Point c{(a.x + b.x) / 2, (a.y + b.y) / 2};
    return Circle{c, max(c.distance(a), c.distance(b))};
}

Circle makeCircumcircle(const Point &a, const Point &b, const Point &c) {
    // Mathematical algorithm from Wikipedia: Circumscribed circle
    double ox = (min(min(a.x, b.x), c.x) + max(min(a.x, b.x), c.x)) / 2;
    double oy = (min(min(a.y, b.y), c.y) + max(min(a.y, b.y), c.y)) / 2;
    double ax = a.x - ox,  ay = a.y - oy;
    double bx = b.x - ox,  by = b.y - oy;
    double cx = c.x - ox,  cy = c.y - oy;
    double d = (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by)) * 2;
    if (d == 0)
        return Circle::INVALID;
    double x = ((ax*ax + ay*ay) * (by - cy) + (bx*bx + by*by) * (cy - ay) + (cx*cx + cy*cy) * (ay - by)) / d;
    double y = ((ax*ax + ay*ay) * (cx - bx) + (bx*bx + by*by) * (ax - cx) + (cx*cx + cy*cy) * (bx - ax)) / d;
    Point p{ox + x, oy + y};
    double r = max(max(p.distance(a), p.distance(b)), p.distance(c));
    return Circle{p, r};
}
