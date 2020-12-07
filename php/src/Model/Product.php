<?php

declare(strict_types=1);

namespace Supermarket\Model;

use Ds\Hashable;

class Product implements Hashable
{
    /**
     * @var string
     */
    private $name;

    /**
     * @var ProductUnit
     */
    private $unit;

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

    /**
     * @param Product $obj
     */
    public function equals($obj): bool
    {
        return $obj instanceof self &&
            $this->getName() === $obj->getName() &&
            $this->getUnit() === $obj->getUnit();
    }

    public function hash()
    {
        return "{$this->getName()}__{$this->getUnit()}";
    }
}
