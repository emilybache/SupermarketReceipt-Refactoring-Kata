<?php

namespace Supermarket\Model;

use Ds\Hashable;

class Product implements Hashable
{
    private string $name;
    private ProductUnit $unit;

    public function __construct(string $name, ProductUnit $unit)
    {
        $this->name = $name;
        $this->unit = $unit;
    }

    public function getName(): string
    {
        return $this->name;
    }

    public function getUnit(): ProductUnit
    {
        return $this->unit;
    }

    public function equals($obj): bool
    {
        return $obj instanceof Product &&
            $this->getName() === $obj->getName() &&
            $this->getUnit() === $obj->getUnit();
    }

    public function hash()
    {
        return "{$this->getName()}__{$this->getUnit()}";
    }
}