<?php

declare(strict_types=1);

namespace Supermarket\Model;

use Ds\Hashable;

class Product implements Hashable
{
    public function __construct(
        private string $name,
        private ProductUnit $unit
    ) {
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

    public function hash(): string
    {
        return "{$this->getName()}__{$this->getUnit()}";
    }
}
